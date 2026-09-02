{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Button where

import Common
import Type
import Engine.Type
import Engine.Underlying
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Sequence as DS
import qualified Data.Vector as DV

create_button_request::ET.Has_call_stack=>(Event a->Engine a->Maybe Int)->(Engine a->Engine a)->Extension_widget_request a->Widget_request a
create_button_request next action button_request=case button_request of
    Button {arrange,window_id,visual_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_pressed_color,outer_pressed_color,inner_hovered_pressed_color,outer_hovered_pressed_color}->case visual_request of
        Text_request {text_width,text_height}->let center_x=arrange.point.x in let center_y=arrange.point.y in let inner_width=text_width/2+inner_thickness in let inner_height=text_height/2+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=button_widget_trigger action,widget_request=Vector_request {index=extension_root_vector_index,vector_widget_request=DS.fromList [Vector_visual_request {arrange=arrange,vector_visual_request=DV.fromList [visual_request,create_rectangle_request center_x center_y inner_color inner_width inner_height,create_rectangle_request center_x center_y outer_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_color outer_width outer_height,create_rectangle_request center_x center_y inner_pressed_color inner_width inner_height,create_rectangle_request center_x center_y outer_pressed_color outer_width outer_height,create_rectangle_request center_x center_y inner_hovered_pressed_color inner_width inner_height,create_rectangle_request center_x center_y outer_hovered_pressed_color outer_width outer_height]},Store_request {store=convert False},Store_request {store=convert False},Store_request {store=convert True},Store_request {store=convert window_id}]}}
        _->EF.empty_error
    _->EF.empty_error

button_widget_trigger::ET.Has_call_stack=>(Engine a->Engine a)->Event a->Engine a->Widget a->(Widget a,Engine a->Engine a)
button_widget_trigger this_action event _ widget=case event of
    At {window_id,action}->case widget of
        Vector {vector_widget}->if window_id==get_store_widget (vector_widget DV.! extension_window_id_index)
            then case action of
                Click {press,mouse_button,x,y}->case mouse_button of
                    Mouse_button_left->case press of
                        Press_down->if above_extension_widget extension_inner_rectangle_index x y (vector_widget DV.! extension_visual_index) then (update_vector_bool (const True) extension_dirty_index (update_vector_bool (const True) extension_state_index widget),id) else (widget,id)
                        Press_up->if view_vector_bool widget extension_state_index then let new_widget=update_vector_bool (const True) extension_dirty_index (update_vector_bool (const False) extension_state_index widget) in if above_extension_widget extension_inner_rectangle_index x y (vector_widget DV.! extension_visual_index) then (new_widget,this_action) else (new_widget,id) else (widget,id)
                    _->(widget,id)
                Move {x,y}->let above=above_extension_widget extension_inner_rectangle_index x y (vector_widget DV.! extension_visual_index) in if above/=view_vector_bool widget extension_hovered_index then (update_vector_bool (const True) extension_dirty_index (update_vector_bool (const above) extension_hovered_index widget),if above then \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (widget,id)
                _->(widget,id)
            else (widget,id)
        _->EF.empty_error
    _->(widget,id)

view_button::ET.Has_call_stack=>Widget a->Widget a
view_button=view_extension_widget

update_button::ET.Has_call_stack=>Widget a->Maybe (Widget a)
update_button=update_extension_widget extension_dirty_index

{-# INLINE create_button_request #-}
{-# INLINE button_widget_trigger #-}
{-# INLINE view_button #-}
{-# INLINE update_button #-}