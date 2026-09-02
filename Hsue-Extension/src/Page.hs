{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Page where

import Common
import Type
import Engine.Text
import Engine.Type
import Engine.Underlying
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad.ST as CMST
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Foreign.C.Types as FCT

update_text::ET.Has_call_stack=>(Visual a->Visual a)->Visual a->(Visual a,Bool)
update_text update visual=case visual of
    Text {current_y=first_current_y}->let new_visual=update visual in case new_visual of
        Text {current_y=second_current_y}->(new_visual,first_current_y/=second_current_y)
        _->EF.empty_error
    _->EF.empty_error

scroll_page::ET.Has_call_stack=>(Visual a->Visual a)->Widget a->Widget a
scroll_page transform widget=case widget of
    Vector {vector_widget}->case vector_widget DV.! extension_visual_index of
        Vector_visual {arrange,vector_visual}->let (new_text_visual,changed)=update_text transform (vector_visual DV.! extension_content_index) in if changed then update_vector_bool (const True) extension_dirty_index (update_vector_widget extension_visual_index (const (Vector_visual {arrange=arrange,vector_visual=DV.modify (\this_vector_visual->DVM.write this_vector_visual extension_content_index new_text_visual) vector_visual})) widget) else widget
        _->EF.empty_error
    _->EF.empty_error

create_page_request::ET.Has_call_stack=>(Event a->Engine a->Maybe Int)->Extension_widget_request a->Widget_request a
create_page_request next page_request=case page_request of
    Page {arrange,window_id,visual_request,step_size,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_selected_color,outer_selected_color,inner_hovered_selected_color,outer_hovered_selected_color}->case visual_request of
        Text_request {text_width,text_height}->let center_x=arrange.point.x in let center_y=arrange.point.y in let inner_width=text_width/2+inner_thickness in let inner_height=text_height/2+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=page_widget_trigger step_size,widget_request=Vector_request {index=extension_root_vector_index,vector_widget_request=DS.fromList [Vector_visual_request {arrange=arrange,vector_visual_request=DV.fromList [visual_request,create_rectangle_request center_x center_y inner_color inner_width inner_height,create_rectangle_request center_x center_y outer_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_color outer_width outer_height,create_rectangle_request center_x center_y inner_selected_color inner_width inner_height,create_rectangle_request center_x center_y outer_selected_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_selected_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_selected_color outer_width outer_height]},Store_request {store=convert False},Store_request {store=convert False},Store_request {store=convert True},Store_request {store=convert window_id}]}}
        _->EF.empty_error
    _->EF.empty_error

page_widget_trigger::ET.Has_call_stack=>FCT.CFloat->Event a->Engine a->Widget a->(Widget a,Engine a->Engine a)
page_widget_trigger step_size event _ widget=case event of
    At {window_id,action}->case widget of
        Vector {vector_widget}->if window_id==get_store_widget (vector_widget DV.! extension_window_id_index)
            then case action of
                Press {press,change}->case press of
                    Press_down->if view_vector_bool widget extension_state_index
                        then case change of
                            Key_down->(scroll_page (scroll_text step_size) widget,id)
                            Key_up->(scroll_page (scroll_text (negate step_size)) widget,id)
                            Key_page_down->(scroll_page scroll_bottom_text widget,id)
                            Key_page_up->(scroll_page scroll_top_text widget,id)
                            _->(widget,id)
                        else (widget,id)
                    _->(widget,id)
                Click {press,mouse_button,x,y}->case mouse_button of
                    Mouse_button_left->case press of
                        Press_down->let above=above_extension_widget extension_inner_rectangle_index x y (vector_widget DV.! extension_visual_index) in if above/=view_vector_bool widget extension_state_index then (update_vector_bool (const True) extension_dirty_index (update_vector_bool (const above) extension_state_index widget),id) else (widget,id)
                        _->(widget,id)
                    Mouse_button_right->case press of
                        Press_down->let above=above_extension_widget extension_inner_rectangle_index x y (vector_widget DV.! extension_visual_index) in if above&&view_vector_bool widget extension_state_index then (update_vector_bool (const True) extension_dirty_index (update_vector_bool (const False) extension_state_index widget),id) else (widget,id)
                        _->(widget,id)
                    _->(widget,id)
                Move {x,y}->let above=above_extension_widget extension_inner_rectangle_index x y (vector_widget DV.! extension_visual_index) in if above/=view_vector_bool widget extension_hovered_index then (update_vector_bool (const True) extension_dirty_index (update_vector_bool (const above) extension_hovered_index widget),if above then \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (widget,id)
                Scroll {x,y,delta_y}->if view_vector_bool widget extension_state_index&&above_extension_widget extension_inner_rectangle_index x y (vector_widget DV.! extension_visual_index) then (scroll_page (scroll_text (negate delta_y*step_size)) widget,id) else (widget,id)
                _->(widget,id)
            else (widget,id)
        _->(widget,id)
    _->(widget,id)

view_page::ET.Has_call_stack=>Widget a->Widget a
view_page=view_extension_widget

update_page::ET.Has_call_stack=>Widget a->Maybe (Widget a)
update_page=update_extension_widget extension_dirty_index

page_getter::ET.Has_call_stack=>Widget a->(FCT.CFloat,FCT.CFloat,FCT.CFloat)
page_getter widget=case extract_extension_widget_vector widget DV.! extension_visual_index of
    Vector_visual {vector_visual}->case vector_visual DV.! extension_content_index of
        Text {half_height,current_y,min_y,max_y}->(max_y-min_y+2*half_height,2*half_height,current_y-min_y)
        _->EF.empty_error
    _->EF.empty_error

page_setter::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Maybe (Widget a->Widget a)
page_setter cached_offset offset=if cached_offset==offset then Nothing else Just (page_setter_a offset)

page_setter_a::ET.Has_call_stack=>FCT.CFloat->Widget a->Widget a
page_setter_a offset=modify_extension_widget (page_setter_b offset)

page_setter_b::ET.Has_call_stack=>FCT.CFloat->Widget a->Widget a
page_setter_b offset widget=case widget of
    Vector {index,vector_widget}->Vector {index=index,vector_widget=CMST.runST (page_setter_c offset vector_widget)}
    _->EF.empty_error

page_setter_c::ET.Has_call_stack=>DVM.PrimMonad b=>FCT.CFloat->DV.Vector (Widget a)->b (DV.Vector (Widget a))
page_setter_c offset vector_widget=do
    new_vector_widget<-DV.thaw vector_widget
    DVM.write new_vector_widget extension_visual_index (page_setter_d offset (vector_widget DV.! extension_visual_index))
    DVM.write new_vector_widget extension_dirty_index (Store {store=convert True})
    DV.unsafeFreeze new_vector_widget

page_setter_d::ET.Has_call_stack=>FCT.CFloat->Widget a->Widget a
page_setter_d offset widget=case widget of
    Vector_visual {arrange,vector_visual}->Vector_visual {arrange=arrange,vector_visual=CMST.runST (page_setter_e offset vector_visual)}
    _->EF.empty_error

page_setter_e::ET.Has_call_stack=>DVM.PrimMonad b=>FCT.CFloat->DV.Vector (Visual a)->b (DV.Vector (Visual a))
page_setter_e offset vector_visual=do
    new_vector_visual<-DV.thaw vector_visual
    DVM.write new_vector_visual extension_content_index (page_setter_f offset (vector_visual DV.! extension_content_index))
    DV.unsafeFreeze new_vector_visual

page_setter_f::ET.Has_call_stack=>FCT.CFloat->Visual a->Visual a
page_setter_f offset visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,anchor,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=min_y+offset,min_y=min_y,max_y=max_y,anchor=anchor,article=article,charset=charset,locked=locked}
    _->EF.empty_error

{-# INLINE update_text #-}
{-# INLINE scroll_page #-}
{-# INLINE create_page_request #-}
{-# INLINE page_widget_trigger #-}
{-# INLINE view_page #-}
{-# INLINE update_page #-}
{-# INLINE page_getter #-}
{-# INLINE page_setter #-}
{-# INLINE page_setter_a #-}
{-# INLINE page_setter_b #-}
{-# INLINE page_setter_c #-}
{-# INLINE page_setter_d #-}
{-# INLINE page_setter_e #-}
{-# INLINE page_setter_f #-}