{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Page where

import Common
import Type
import Engine.Operation
import Engine.Text
import Engine.Type
import qualified Error.Error as EE
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Foreign.C.Types as FCT

extension_page_visual_index::Int
extension_page_visual_index=0

extension_page_hovered_index::Int
extension_page_hovered_index=1

extension_page_selected_index::Int
extension_page_selected_index=2

extension_page_dirty_index::Int
extension_page_dirty_index=3

extension_page_window_id_index::Int
extension_page_window_id_index=4

extension_page_text_visual_index::Int
extension_page_text_visual_index=0

extension_page_inner_rectangle_index::Int
extension_page_inner_rectangle_index=1

extension_page_root_vector_index::Int
extension_page_root_vector_index=0

extension_page_unselected_unhovered_offset::Int
extension_page_unselected_unhovered_offset=0

extension_page_unselected_hovered_offset::Int
extension_page_unselected_hovered_offset=2

extension_page_selected_unhovered_offset::Int
extension_page_selected_unhovered_offset=4

extension_page_selected_hovered_offset::Int
extension_page_selected_hovered_offset=6

extension_page_inner_rectangle_base_offset::Int
extension_page_inner_rectangle_base_offset=1

extension_page_outer_rectangle_base_offset::Int
extension_page_outer_rectangle_base_offset=2

update_text::(Visual->Visual)->Visual->(Visual,Bool)
update_text update visual=case visual of
    Text {current_y=first_y}->let new_visual=update visual in case new_visual of
        Text {current_y=second_y}->(new_visual,first_y/=second_y)
        _->EE.quick_error "update_text" 0
    _->EE.quick_error "update_text" 1

scroll_page::(Visual->Visual)->Widget a b c d e->Widget a b c d e
scroll_page transform widget=case widget of
    Vector {vector_widget}->case vector_widget DV.! extension_page_visual_index of
        Vector_visual {arrange,collect_order,size=visual_size,vector_visual}->let (new_text_visual,changed)=update_text transform (vector_visual DV.! extension_page_text_visual_index) in if changed then update_vector_bool (const True) extension_page_dirty_index (update_vector_widget extension_page_visual_index (const (Vector_visual {arrange=arrange,collect_order=collect_order,size=visual_size,vector_visual=DV.modify (\this_vector_widget->DVM.write this_vector_widget extension_page_text_visual_index new_text_visual) vector_visual})) widget) else widget
        _->EE.quick_error "scroll_page" 0
    _->EE.quick_error "scroll_page" 1

create_page_request::(Event a->Engine b a c d e->Maybe Int)->(Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,Engine b a c d e->Engine b a c d e))->Extension_widget_request b a c d e->Widget_request b a c d e
create_page_request next widget_trigger page_request=case page_request of
    Page {window_id,arrange,visual_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_selected_color,outer_selected_color,inner_hovered_selected_color,outer_hovered_selected_color}->case visual_request of
        Text_request {text_width,text_height}->let center_x=arrange.point.x in let center_y=arrange.point.y in let inner_width=text_width/2+inner_thickness in let inner_height=text_height/2+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=widget_trigger,widget_request=Vector_request {index=extension_page_root_vector_index,vector_widget_request=DS.singleton (Vector_visual_request {arrange=arrange,collect_order=extension_page_outer_rectangle_base_offset DS.<| extension_page_inner_rectangle_base_offset DS.<| DS.singleton extension_page_text_visual_index,vector_visual_request=DV.fromList [visual_request,create_rectangle_request center_x center_y inner_color inner_width inner_height,create_rectangle_request center_x center_y outer_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_color outer_width outer_height,create_rectangle_request center_x center_y inner_selected_color inner_width inner_height,create_rectangle_request center_x center_y outer_selected_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_selected_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_selected_color outer_width outer_height]}) DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True} DS.|> Store_request {store=convert window_id}}}
        _->EE.quick_error "create_page_request" 0
    _->EE.quick_error "create_page_request" 1

page_widget_trigger::FCT.CFloat->Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)
page_widget_trigger step_size event _ widget=case event of
    At {window_id,action}->case widget of
        Vector {vector_widget}->if window_id==get_store_widget (vector_widget DV.! extension_page_window_id_index)
            then case action of
                Press {press,change}->case press of
                    Press_down->if view_vector_bool widget extension_page_selected_index 
                        then case change of
                            Key_down->(scroll_page (scroll_text step_size) widget,id)
                            Key_up->(scroll_page (scroll_text (negate step_size)) widget,id)
                            Key_page_down->(scroll_page scroll_bottom_text widget,id)
                            Key_page_up->(scroll_page scroll_top_text widget,id)
                            _->(widget,id)
                        else (widget,id)
                    _->(widget,id)
                Click {press,mouse_button,x=x,y=y}->case mouse_button of
                    Mouse_button_left->case press of
                        Press_down->let above=above_extension_widget extension_page_inner_rectangle_index x y (vector_widget DV.! extension_page_visual_index) in if above/=view_vector_bool widget extension_page_selected_index then (update_vector_bool (const True) extension_page_dirty_index (update_vector_bool (const above) extension_page_selected_index widget),id) else (widget,id)
                        _->(widget,id)
                    _->(widget,id)
                Move {x,y}->let above=above_extension_widget extension_page_inner_rectangle_index x y (vector_widget DV.! extension_page_visual_index) in if above/=view_vector_bool widget extension_page_hovered_index then (update_vector_bool (const True) extension_page_dirty_index (update_vector_bool (const above) extension_page_hovered_index widget),if above then \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (widget,id)
                Scroll {x,y,delta_y}->if above_extension_widget extension_page_inner_rectangle_index x y (vector_widget DV.! extension_page_visual_index) then (scroll_page (scroll_text (negate delta_y*step_size)) widget,id) else (widget,id)
                _->(widget,id)
            else (widget,id)
        _->(widget,id)
    _->(widget,id)

view_page::Widget a b c d e->Widget a b c d e
view_page=view_extension_widget (view_extension_visual extension_page_visual_index extension_page_hovered_index extension_page_selected_index extension_page_selected_hovered_offset extension_page_selected_unhovered_offset extension_page_unselected_hovered_offset extension_page_unselected_unhovered_offset extension_page_outer_rectangle_base_offset extension_page_inner_rectangle_base_offset extension_page_text_visual_index)

update_page::Widget a b c d e->Maybe (Widget a b c d e)
update_page=update_extension_widget extension_page_dirty_index

{-# INLINE extension_page_visual_index #-}
{-# INLINE extension_page_hovered_index #-}
{-# INLINE extension_page_selected_index #-}
{-# INLINE extension_page_dirty_index #-}
{-# INLINE extension_page_window_id_index #-}
{-# INLINE extension_page_text_visual_index #-}
{-# INLINE extension_page_inner_rectangle_index #-}
{-# INLINE extension_page_root_vector_index #-}
{-# INLINE extension_page_unselected_unhovered_offset #-}
{-# INLINE extension_page_unselected_hovered_offset #-}
{-# INLINE extension_page_selected_unhovered_offset #-}
{-# INLINE extension_page_selected_hovered_offset #-}
{-# INLINE extension_page_inner_rectangle_base_offset #-}
{-# INLINE extension_page_outer_rectangle_base_offset #-}
{-# INLINE update_text #-}
{-# INLINE scroll_page #-}
{-# INLINE create_page_request #-}
{-# INLINE page_widget_trigger #-}
{-# INLINE view_page #-}
{-# INLINE update_page #-}