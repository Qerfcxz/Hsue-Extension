{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Slider where

import Common
import Type
import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad.ST as CMST
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Foreign.C.Types as FCT

extension_slider_visual_index::ET.Has_call_stack=>Int
extension_slider_visual_index=0

extension_slider_window_id_index::ET.Has_call_stack=>Int
extension_slider_window_id_index=1

extension_slider_first_triangle_state_index::ET.Has_call_stack=>Int
extension_slider_first_triangle_state_index=2

extension_slider_second_triangle_state_index::ET.Has_call_stack=>Int
extension_slider_second_triangle_state_index=3

extension_slider_thumb_state_index::ET.Has_call_stack=>Int
extension_slider_thumb_state_index=4

extension_slider_cached_offset_index::ET.Has_call_stack=>Int
extension_slider_cached_offset_index=5

extension_slider_drag_start_position_index::ET.Has_call_stack=>Int
extension_slider_drag_start_position_index=6

extension_slider_drag_start_offset_index::ET.Has_call_stack=>Int
extension_slider_drag_start_offset_index=7

extension_slider_thumb_length_index::ET.Has_call_stack=>Int
extension_slider_thumb_length_index=8

extension_slider_thumb_position_index::ET.Has_call_stack=>Int
extension_slider_thumb_position_index=9

extension_slider_horizontal_index::ET.Has_call_stack=>Int
extension_slider_horizontal_index=10

extension_slider_min_thumb_length_index::ET.Has_call_stack=>Int
extension_slider_min_thumb_length_index=11

extension_slider_x_index::ET.Has_call_stack=>Int
extension_slider_x_index=12

extension_slider_y_index::ET.Has_call_stack=>Int
extension_slider_y_index=13

extension_slider_dirty_index::ET.Has_call_stack=>Int
extension_slider_dirty_index=14

extension_slider_first_triangle_visual_base_index::ET.Has_call_stack=>Int
extension_slider_first_triangle_visual_base_index=2

extension_slider_second_triangle_visual_base_index::ET.Has_call_stack=>Int
extension_slider_second_triangle_visual_base_index=5

extension_slider_thumb_visual_base_index::ET.Has_call_stack=>Int
extension_slider_thumb_visual_base_index=8

extension_slider_outer_rectangle_visual_index::ET.Has_call_stack=>Int
extension_slider_outer_rectangle_visual_index=0

extension_slider_inner_rectangle_visual_index::ET.Has_call_stack=>Int
extension_slider_inner_rectangle_visual_index=1

extension_slider_state_normal::ET.Has_call_stack=>Int
extension_slider_state_normal=0

extension_slider_state_hovered::ET.Has_call_stack=>Int
extension_slider_state_hovered=1

extension_slider_state_pressed::ET.Has_call_stack=>Int
extension_slider_state_pressed=2

extension_slider_horizontal_flag_false::ET.Has_call_stack=>Int
extension_slider_horizontal_flag_false=0

extension_slider_horizontal_flag_true::ET.Has_call_stack=>Int
extension_slider_horizontal_flag_true=1

above_thumb::ET.Has_call_stack=>Bool->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_thumb horizontal x y thumb_position thumb_length radius this_x this_y=if horizontal then above_box x y thumb_position this_y (thumb_length/2) radius else above_box x y this_x thumb_position radius (thumb_length/2)

calculate_thumb_length::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat
calculate_thumb_length content_size viewport_size min_thumb_length track_geometric_length=if content_size<=0 then track_geometric_length else max min_thumb_length (track_geometric_length*viewport_size/content_size)

calculate_thumb_position::ET.Has_call_stack=>Bool->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat
calculate_thumb_position horizontal content_size viewport_size offset track_start_position track_end_position thumb_length=if content_size<=viewport_size then track_start_position+(track_end_position-track_start_position)/2 else if horizontal then track_start_position+(track_end_position-track_start_position-thumb_length)*offset/(content_size-viewport_size)+thumb_length/2 else track_start_position+(track_end_position-track_start_position+thumb_length)*offset/(content_size-viewport_size)-thumb_length/2

create_slider_request::ET.Has_call_stack=>(Event a->Engine a->Maybe Int)->Extension_widget_request a->Widget_request a
create_slider_request next slider_request=case slider_request of
    Slider {arrange,window_id,leaf_id,selector,getter,setter,x,y,width,height,step_size,min_thumb_length,inner_thickness,outer_thickness,inner_color,outer_color,triangle_color,triangle_hovered_color,triangle_pressed_color,thumb_color,thumb_hovered_color,thumb_pressed_color,horizontal}->let center_x=arrange.point.x+x in let center_y=arrange.point.y+y in let half_width=width/2 in let half_height=height/2 in let inner_half_width=half_width-outer_thickness in let inner_half_height=half_height-outer_thickness in let radius=if horizontal then inner_half_height-inner_thickness else inner_half_width-inner_thickness in let first_triangle_center_x=if horizontal then center_x-inner_half_width+2*inner_thickness+radius else center_x in let first_triangle_center_y=if horizontal then center_y else center_y+inner_half_height-2*inner_thickness-radius in let second_triangle_center_x=if horizontal then center_x+inner_half_width-2*inner_thickness-radius else center_x in let second_triangle_center_y=if horizontal then center_y else center_y-inner_half_height+2*inner_thickness+radius in let first_triangle_first_point=if horizontal then Point {x=negate radius,y=0} else Point {x=0,y=radius} in let first_triangle_second_point=if horizontal then Point {x=radius,y=radius} else Point {x=negate radius,y=negate radius} in let first_triangle_third_point=Point {x=radius,y=negate radius} in let second_triangle_first_point=if horizontal then Point {x=radius,y=0} else Point {x=0,y=negate radius} in let second_triangle_second_point=Point {x=negate radius,y=radius} in let second_triangle_third_point=if horizontal then Point {x=negate radius,y=negate radius} else Point {x=radius,y=radius} in let thumb_base_half_width=if horizontal then 1 else radius in let thumb_base_half_height=if horizontal then radius else 1 in let track_start_position=if horizontal then center_x-inner_half_width+3*inner_thickness+2*radius else center_y+inner_half_height-3*inner_thickness-2*radius in let track_end_position=if horizontal then center_x+inner_half_width-3*inner_thickness-2*radius else center_y-inner_half_height+3*inner_thickness+2*radius in Widget_trigger_request {next=next,widget_trigger=slider_widget_trigger leaf_id selector getter setter center_x center_y radius track_start_position track_end_position step_size first_triangle_center_x first_triangle_center_y second_triangle_center_x second_triangle_center_y horizontal,widget_request=Vector_request {index=extension_root_vector_index,vector_widget_request=DS.fromList [Vector_visual_request {arrange=arrange {point=Point {x=center_x,y=center_y}},vector_visual_request=DV.fromList [create_rectangle_request center_x center_y outer_color half_width half_height,create_rectangle_request center_x center_y inner_color inner_half_width inner_half_height,create_triangle_request first_triangle_center_x first_triangle_center_y triangle_color first_triangle_first_point first_triangle_second_point first_triangle_third_point,create_triangle_request first_triangle_center_x first_triangle_center_y triangle_hovered_color first_triangle_first_point first_triangle_second_point first_triangle_third_point,create_triangle_request first_triangle_center_x first_triangle_center_y triangle_pressed_color first_triangle_first_point first_triangle_second_point first_triangle_third_point,create_triangle_request second_triangle_center_x second_triangle_center_y triangle_color second_triangle_first_point second_triangle_second_point second_triangle_third_point,create_triangle_request second_triangle_center_x second_triangle_center_y triangle_hovered_color second_triangle_first_point second_triangle_second_point second_triangle_third_point,create_triangle_request second_triangle_center_x second_triangle_center_y triangle_pressed_color second_triangle_first_point second_triangle_second_point second_triangle_third_point,create_rectangle_request center_x center_y thumb_color thumb_base_half_width thumb_base_half_height,create_rectangle_request center_x center_y thumb_hovered_color thumb_base_half_width thumb_base_half_height,create_rectangle_request center_x center_y thumb_pressed_color thumb_base_half_width thumb_base_half_height]},Store_request {store=convert window_id},Store_request {store=convert extension_slider_state_normal},Store_request {store=convert extension_slider_state_normal},Store_request {store=convert extension_slider_state_normal},Store_request {store=convert (0::FCT.CFloat)},Store_request {store=convert (0::FCT.CFloat)},Store_request {store=convert (0::FCT.CFloat)},Store_request {store=convert (abs (track_end_position-track_start_position))},Store_request {store=convert track_start_position},Store_request {store=convert (if horizontal then extension_slider_horizontal_flag_true else extension_slider_horizontal_flag_false)},Store_request {store=convert min_thumb_length},Store_request {store=convert center_x},Store_request {store=convert center_y},Store_request {store=convert True}]}}
    _->EF.empty_error

slider_widget_trigger::ET.Has_call_stack=>Int->Selector ()->(Widget a->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->(FCT.CFloat->FCT.CFloat->Maybe (Widget a->Widget a))->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool->Event a->Engine a->Widget a->(Widget a,Engine a->Engine a)
slider_widget_trigger leaf_id selector getter setter this_x this_y radius track_start_position track_end_position step_size first_triangle_center_x first_triangle_center_y second_triangle_center_x second_triangle_center_y horizontal event engine widget=case event of
    At {window_id,action}->case widget of
        Vector {vector_widget}->if window_id==get_store_widget (vector_widget DV.! extension_slider_window_id_index)
            then let first_triangle_state=get_store_widget (vector_widget DV.! extension_slider_first_triangle_state_index) in let second_triangle_state=get_store_widget (vector_widget DV.! extension_slider_second_triangle_state_index) in let thumb_state=get_store_widget (vector_widget DV.! extension_slider_thumb_state_index) in let cached_offset=get_store_widget (vector_widget DV.! extension_slider_cached_offset_index) in let drag_start_position=get_store_widget (vector_widget DV.! extension_slider_drag_start_position_index) in let thumb_length=get_store_widget (vector_widget DV.! extension_slider_thumb_length_index) in let thumb_position=get_store_widget (vector_widget DV.! extension_slider_thumb_position_index) in let (content_size,viewport_size,current_offset)=getter (selector_action (\_ this_widget _->this_widget) selector (lookup_projection_widget (Object_path {leaf_id=leaf_id}) engine) (lookup_projection_widget (Object_path {leaf_id=leaf_id}) engine)) in let track_geometric_length=abs (track_end_position-track_start_position) in let new_thumb_length=calculate_thumb_length content_size viewport_size (get_store_widget (vector_widget DV.! extension_slider_min_thumb_length_index)) track_geometric_length in let new_thumb_position=calculate_thumb_position horizontal content_size viewport_size current_offset track_start_position track_end_position new_thumb_length in case action of
                Click {press,mouse_button,x,y}->case mouse_button of
                    Mouse_button_left->case press of
                        Press_down->let hit_first_triangle_flag=above_triangle x y first_triangle_center_x first_triangle_center_y radius in let hit_second_triangle_flag=above_triangle x y second_triangle_center_x second_triangle_center_y radius in let hit_thumb_flag=above_thumb horizontal x y thumb_position thumb_length radius this_x this_y in if hit_first_triangle_flag then let new_offset=max 0 (cached_offset-step_size) in let new_widget=set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const (calculate_thumb_position horizontal content_size viewport_size new_offset track_start_position track_end_position new_thumb_length))) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const new_offset)) (update_vector_widget extension_slider_first_triangle_state_index (update_store_widget (const extension_slider_state_pressed)) widget)))) in if cached_offset/=new_offset
                            then case setter cached_offset new_offset of
                                Just update_function->(new_widget,\this_engine->this_engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const update_function) selector)) this_engine.leaf})
                                Nothing->(new_widget,id)
                            else (new_widget,id) else if hit_second_triangle_flag then let new_offset=min (max 0 (content_size-viewport_size)) (cached_offset+step_size) in let new_widget=set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const (calculate_thumb_position horizontal content_size viewport_size new_offset track_start_position track_end_position new_thumb_length))) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const new_offset)) (update_vector_widget extension_slider_second_triangle_state_index (update_store_widget (const extension_slider_state_pressed)) widget)))) in if cached_offset/=new_offset
                                then case setter cached_offset new_offset of
                                    Just update_function->(new_widget,\this_engine->this_engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const update_function) selector)) this_engine.leaf})
                                    Nothing->(new_widget,id)
                                else (new_widget,id) else if hit_thumb_flag then (set_slider_updated (update_vector_widget extension_slider_drag_start_offset_index (update_store_widget (const cached_offset)) (update_vector_widget extension_slider_drag_start_position_index (update_store_widget (const (if horizontal then x else y))) (update_vector_widget extension_slider_thumb_state_index (update_store_widget (const extension_slider_state_pressed)) widget))),id) else let track_half_length=abs (track_end_position-track_start_position)/2 in let track_center=track_start_position+(track_end_position-track_start_position)/2 in if above_box x y (if horizontal then track_center else this_x) (if horizontal then this_y else track_center) (if horizontal then track_half_length else radius) (if horizontal then radius else track_half_length) then let start_center=track_start_position+(if horizontal then new_thumb_length/2 else -new_thumb_length/2) in let end_center=track_end_position+(if horizontal then -new_thumb_length/2 else new_thumb_length/2) in let travel_distance=if horizontal then end_center-start_center else start_center-end_center in let click_position=if horizontal then x else y in let ratio=max 0 (min 1 (if travel_distance==0 then 0 else (if horizontal then click_position-start_center else start_center-click_position)/travel_distance)) in let new_offset=ratio*max 0 (content_size-viewport_size) in let new_thumb_position_after_jump=calculate_thumb_position horizontal content_size viewport_size new_offset track_start_position track_end_position new_thumb_length in let hit_thumb_after_jump=above_thumb horizontal x y new_thumb_position_after_jump new_thumb_length radius this_x this_y in let new_thumb_state=if hit_thumb_after_jump then extension_slider_state_hovered else extension_slider_state_normal in let new_widget=set_slider_updated (update_vector_widget extension_slider_thumb_state_index (update_store_widget (const new_thumb_state)) (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const new_thumb_position_after_jump)) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const new_offset)) widget)))) in let cursor_action=if thumb_state==extension_slider_state_normal&&hit_thumb_after_jump then \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else id in if cached_offset/=new_offset
                                    then case setter cached_offset new_offset of
                                        Just update_function->(new_widget,\this_engine->cursor_action (this_engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const update_function) selector)) this_engine.leaf}))
                                        Nothing->(new_widget,cursor_action)
                                    else (new_widget,cursor_action) else (widget,id)
                        Press_up->let hit_first_triangle_flag=above_triangle x y first_triangle_center_x first_triangle_center_y radius in let hit_second_triangle_flag=above_triangle x y second_triangle_center_x second_triangle_center_y radius in let hit_thumb_flag=above_thumb horizontal x y thumb_position thumb_length radius this_x this_y in let was_hovered=extension_slider_state_normal<first_triangle_state||extension_slider_state_normal<second_triangle_state||extension_slider_state_normal<thumb_state in let is_hovered=hit_first_triangle_flag||hit_second_triangle_flag||hit_thumb_flag in let cursor_action=if was_hovered/=is_hovered then if is_hovered then \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}} else id in (set_slider_updated (update_vector_widget extension_slider_thumb_state_index (update_store_widget (const (if hit_thumb_flag then extension_slider_state_hovered else extension_slider_state_normal))) (update_vector_widget extension_slider_second_triangle_state_index (update_store_widget (const (if hit_second_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal))) (update_vector_widget extension_slider_first_triangle_state_index (update_store_widget (const (if hit_first_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal))) widget))),cursor_action)
                    _->(widget,id)
                Move {x,y}->if thumb_state==extension_slider_state_pressed
                    then let scrollable=content_size-viewport_size in let track_movable_length=max 0 (track_geometric_length-new_thumb_length) in let final_offset=max 0 (min scrollable (get_store_widget (vector_widget DV.! extension_slider_drag_start_offset_index)+(if track_movable_length==0 then 0 else (if horizontal then x-drag_start_position else drag_start_position-y)/track_movable_length*scrollable))) in let new_widget=set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const (calculate_thumb_position horizontal content_size viewport_size final_offset track_start_position track_end_position new_thumb_length))) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const final_offset)) widget))) in case setter cached_offset final_offset of
                        Just update_function->(new_widget,\this_engine->this_engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const update_function) selector)) this_engine.leaf})
                        Nothing->(new_widget,id)
                    else let hit_first_triangle_flag=above_triangle x y first_triangle_center_x first_triangle_center_y radius in let hit_second_triangle_flag=above_triangle x y second_triangle_center_x second_triangle_center_y radius in let hit_thumb_flag=above_thumb horizontal x y new_thumb_position new_thumb_length radius this_x this_y in let new_first_triangle_state=if first_triangle_state==extension_slider_state_pressed then extension_slider_state_pressed else if hit_first_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal in let new_second_triangle_state=if second_triangle_state==extension_slider_state_pressed then extension_slider_state_pressed else if hit_second_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal in let new_thumb_state=if thumb_state==extension_slider_state_pressed then extension_slider_state_pressed else if hit_thumb_flag then extension_slider_state_hovered else extension_slider_state_normal in let state_changed=first_triangle_state/=new_first_triangle_state||second_triangle_state/=new_second_triangle_state||thumb_state/=new_thumb_state||current_offset/=cached_offset||new_thumb_length/=thumb_length||new_thumb_position/=thumb_position in let was_hovered=extension_slider_state_normal<first_triangle_state||extension_slider_state_normal<second_triangle_state||extension_slider_state_normal<thumb_state in let is_hovered=hit_first_triangle_flag||hit_second_triangle_flag||hit_thumb_flag in let cursor_action=if was_hovered/=is_hovered then if is_hovered then \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}} else id in if state_changed then (set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const new_thumb_position)) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const current_offset)) (update_vector_widget extension_slider_thumb_state_index (update_store_widget (const new_thumb_state)) (update_vector_widget extension_slider_second_triangle_state_index (update_store_widget (const new_second_triangle_state)) (update_vector_widget extension_slider_first_triangle_state_index (update_store_widget (const new_first_triangle_state)) widget)))))),cursor_action) else (widget,id)
                _->if current_offset/=cached_offset||new_thumb_position/=thumb_position||new_thumb_length/=thumb_length then (set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const new_thumb_position)) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const current_offset)) widget))),id) else (widget,id)
            else (widget,id)
        _->(widget,id)
    _->(widget,id)

set_slider_updated::ET.Has_call_stack=>Widget a->Widget a
set_slider_updated=update_vector_bool (const True) extension_slider_dirty_index

update_thumb_arrange::ET.Has_call_stack=>Bool->FCT.CFloat->FCT.CFloat->FCT.CFloat->Arrange->Arrange
update_thumb_arrange horizontal thumb_position x y arrange=case arrange of
    Arrange {matrix,color}->Arrange {point=Point {x=if horizontal then thumb_position else x,y=if horizontal then y else thumb_position},matrix=matrix,color=color}

view_slider::ET.Has_call_stack=>Widget a->Widget a
view_slider widget=view_slider_a (extract_extension_widget_vector widget)

view_slider_a::ET.Has_call_stack=>DV.Vector (Widget a)->Widget a
view_slider_a vector_widget=case vector_widget DV.! extension_slider_visual_index of
    Vector_visual {arrange=first_arrange,vector_visual}->let thumb_state=get_store_widget (vector_widget DV.! extension_slider_thumb_state_index) in let thumb_length=get_store_widget (vector_widget DV.! extension_slider_thumb_length_index) in let thumb_position=get_store_widget (vector_widget DV.! extension_slider_thumb_position_index) in let horizontal=get_store_widget (vector_widget DV.! extension_slider_horizontal_index)==extension_slider_horizontal_flag_true in let x=get_store_widget (vector_widget DV.! extension_slider_x_index) in let y=get_store_widget (vector_widget DV.! extension_slider_y_index) in let thumb_visual=vector_visual DV.! (extension_slider_thumb_visual_base_index+thumb_state) in case thumb_visual of
        Rectangle {arrange=second_arrange,half_width,half_height}->let new_vector_visual=CMST.runST (action_vector (\this_vector_visual->DVM.write this_vector_visual (extension_slider_thumb_visual_base_index+thumb_state) (Rectangle {arrange=update_thumb_arrange horizontal thumb_position x y second_arrange,half_width=if horizontal then thumb_length/2 else half_width,half_height=if horizontal then half_height else thumb_length/2})) vector_visual) in Vector_visual {arrange=first_arrange,vector_visual=new_vector_visual}
        _->EF.empty_error
    _->EF.empty_error

update_slider::ET.Has_call_stack=>Widget a->Maybe (Widget a)
update_slider=update_extension_widget extension_slider_dirty_index

{-# INLINE extension_slider_visual_index #-}
{-# INLINE extension_slider_window_id_index #-}
{-# INLINE extension_slider_first_triangle_state_index #-}
{-# INLINE extension_slider_second_triangle_state_index #-}
{-# INLINE extension_slider_thumb_state_index #-}
{-# INLINE extension_slider_cached_offset_index #-}
{-# INLINE extension_slider_drag_start_position_index #-}
{-# INLINE extension_slider_drag_start_offset_index #-}
{-# INLINE extension_slider_thumb_length_index #-}
{-# INLINE extension_slider_thumb_position_index #-}
{-# INLINE extension_slider_horizontal_index #-}
{-# INLINE extension_slider_min_thumb_length_index #-}
{-# INLINE extension_slider_x_index #-}
{-# INLINE extension_slider_y_index #-}
{-# INLINE extension_slider_dirty_index #-}
{-# INLINE extension_slider_first_triangle_visual_base_index #-}
{-# INLINE extension_slider_second_triangle_visual_base_index #-}
{-# INLINE extension_slider_thumb_visual_base_index #-}
{-# INLINE extension_slider_outer_rectangle_visual_index #-}
{-# INLINE extension_slider_inner_rectangle_visual_index #-}
{-# INLINE extension_slider_state_normal #-}
{-# INLINE extension_slider_state_hovered #-}
{-# INLINE extension_slider_state_pressed #-}
{-# INLINE extension_slider_horizontal_flag_false #-}
{-# INLINE extension_slider_horizontal_flag_true #-}
{-# INLINE above_thumb #-}
{-# INLINE calculate_thumb_length #-}
{-# INLINE calculate_thumb_position #-}
{-# INLINE create_slider_request #-}
{-# INLINE slider_widget_trigger #-}
{-# INLINE set_slider_updated #-}
{-# INLINE update_thumb_arrange #-}
{-# INLINE view_slider #-}
{-# INLINE view_slider_a #-}
{-# INLINE update_slider #-}