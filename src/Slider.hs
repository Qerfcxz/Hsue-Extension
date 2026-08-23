{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Slider where

import Common
import Page
import Engine.Container
import Engine.Operation
import Engine.Projection
import Engine.Selector
import Engine.Type
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

slider_widget_trigger::ET.Has_call_stack=>Int->Selector ()->(Widget a b c d e->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->(FCT.CFloat->FCT.CFloat->Maybe (Widget a b c d e->Widget a b c d e))->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool->Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)
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
                                else (new_widget,id) else if hit_thumb_flag then (set_slider_updated (update_vector_widget extension_slider_drag_start_offset_index (update_store_widget (const cached_offset)) (update_vector_widget extension_slider_drag_start_position_index (update_store_widget (const (if horizontal then x else y))) (update_vector_widget extension_slider_thumb_state_index (update_store_widget (const extension_slider_state_pressed)) widget))),id) else let track_half_length=abs (track_end_position-track_start_position)/2 in let track_center=track_start_position+(track_end_position-track_start_position)/2 in if above_box x y (if horizontal then track_center else this_x) (if horizontal then this_y else track_center) (if horizontal then track_half_length else radius) (if horizontal then radius else track_half_length) then let new_offset=if if horizontal then x<thumb_position else y>thumb_position then max 0 (cached_offset-viewport_size) else min (max 0 (content_size-viewport_size)) (cached_offset+viewport_size) in let new_widget=set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const (calculate_thumb_position horizontal content_size viewport_size new_offset track_start_position track_end_position new_thumb_length))) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const new_offset)) widget))) in if cached_offset/=new_offset
                                    then case setter cached_offset new_offset of
                                        Just update_function->(new_widget,\this_engine->this_engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const update_function) selector)) this_engine.leaf})
                                        Nothing->(new_widget,id)
                                    else (new_widget,id) else (widget,id)
                        Press_up->let hit_first_triangle_flag=above_triangle x y first_triangle_center_x first_triangle_center_y radius in let hit_second_triangle_flag=above_triangle x y second_triangle_center_x second_triangle_center_y radius in let hit_thumb_flag=above_thumb horizontal x y thumb_position thumb_length radius this_x this_y in (set_slider_updated (update_vector_widget extension_slider_thumb_state_index (update_store_widget (const (if hit_thumb_flag then extension_slider_state_hovered else extension_slider_state_normal))) (update_vector_widget extension_slider_second_triangle_state_index (update_store_widget (const (if hit_second_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal))) (update_vector_widget extension_slider_first_triangle_state_index (update_store_widget (const (if hit_first_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal))) widget))),id)
                    _->(widget,id)
                Move {x,y}->if thumb_state==extension_slider_state_pressed
                    then let scrollable=content_size-viewport_size in let track_movable_length=max 0 (track_geometric_length-new_thumb_length) in let final_offset=max 0 (min scrollable (get_store_widget (vector_widget DV.! extension_slider_drag_start_offset_index)+(if track_movable_length==0 then 0 else (if horizontal then x-drag_start_position else drag_start_position-y)/track_movable_length*scrollable))) in let new_widget=set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const (calculate_thumb_position horizontal content_size viewport_size final_offset track_start_position track_end_position new_thumb_length))) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const final_offset)) widget))) in case setter cached_offset final_offset of
                        Just update_function->(new_widget,\this_engine->this_engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const update_function) selector)) this_engine.leaf})
                        Nothing->(new_widget,id)
                    else let hit_first_triangle_flag=above_triangle x y first_triangle_center_x first_triangle_center_y radius in let hit_second_triangle_flag=above_triangle x y second_triangle_center_x second_triangle_center_y radius in let hit_thumb_flag=above_thumb horizontal x y new_thumb_position new_thumb_length radius this_x this_y in let new_first_triangle_state=if first_triangle_state==extension_slider_state_pressed then extension_slider_state_pressed else if hit_first_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal in let new_second_triangle_state=if second_triangle_state==extension_slider_state_pressed then extension_slider_state_pressed else if hit_second_triangle_flag then extension_slider_state_hovered else extension_slider_state_normal in let new_thumb_state=if thumb_state==extension_slider_state_pressed then extension_slider_state_pressed else if hit_thumb_flag then extension_slider_state_hovered else extension_slider_state_normal in let state_changed=first_triangle_state/=new_first_triangle_state||second_triangle_state/=new_second_triangle_state||thumb_state/=new_thumb_state||current_offset/=cached_offset||new_thumb_length/=thumb_length||new_thumb_position/=thumb_position in let updated_widget=set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const new_thumb_position)) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const current_offset)) (update_vector_widget extension_slider_thumb_state_index (update_store_widget (const new_thumb_state)) (update_vector_widget extension_slider_second_triangle_state_index (update_store_widget (const new_second_triangle_state)) (update_vector_widget extension_slider_first_triangle_state_index (update_store_widget (const new_first_triangle_state)) widget)))))) in let was_hovered=extension_slider_state_normal<first_triangle_state||extension_slider_state_normal<second_triangle_state||extension_slider_state_normal<thumb_state in let is_hovered=hit_first_triangle_flag||hit_second_triangle_flag||hit_thumb_flag in let cursor_action=if was_hovered/=is_hovered then if is_hovered then \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}} else id in if state_changed then (updated_widget,cursor_action) else (widget,id)
                _->if current_offset/=cached_offset||new_thumb_position/=thumb_position||new_thumb_length/=thumb_length then (set_slider_updated (update_vector_widget extension_slider_thumb_position_index (update_store_widget (const new_thumb_position)) (update_vector_widget extension_slider_thumb_length_index (update_store_widget (const new_thumb_length)) (update_vector_widget extension_slider_cached_offset_index (update_store_widget (const current_offset)) widget))),id) else (widget,id)
            else (widget,id)
        _->(widget,id)
    _->(widget,id)

set_slider_updated::ET.Has_call_stack=>Widget a b c d e->Widget a b c d e
set_slider_updated=update_vector_bool (const True) extension_slider_dirty_index

update_thumb_arrange::ET.Has_call_stack=>Bool->FCT.CFloat->FCT.CFloat->FCT.CFloat->Arrange->Arrange
update_thumb_arrange horizontal thumb_position x y arrange=case arrange of
    Arrange {matrix,color}->Arrange {point=Point {x=if horizontal then thumb_position else x,y=if horizontal then y else thumb_position},matrix=matrix,color=color}

view_slider::ET.Has_call_stack=>Widget a b c d e->Widget a b c d e
view_slider=view_extension_widget view_slider_a

view_slider_a::ET.Has_call_stack=>DV.Vector (Widget a b c d e)->Widget a b c d e
view_slider_a vector_widget=case vector_widget DV.! extension_slider_visual_index of
    Vector_visual {arrange=first_arrange,vector_visual,size}->let thumb_state=get_store_widget (vector_widget DV.! extension_slider_thumb_state_index) in let thumb_length=get_store_widget (vector_widget DV.! extension_slider_thumb_length_index) in let thumb_position=get_store_widget (vector_widget DV.! extension_slider_thumb_position_index) in let horizontal=get_store_widget (vector_widget DV.! extension_slider_horizontal_index)==extension_slider_horizontal_flag_true in let x=get_store_widget (vector_widget DV.! extension_slider_x_index) in let y=get_store_widget (vector_widget DV.! extension_slider_y_index) in let thumb_visual=vector_visual DV.! (extension_slider_thumb_visual_base_index+thumb_state) in case thumb_visual of
        Rectangle {arrange=second_arrange,half_width,half_height}->let new_vector_visual=DV.modify (\this_vector_widget->DVM.write this_vector_widget (extension_slider_thumb_visual_base_index+thumb_state) (Rectangle {arrange=update_thumb_arrange horizontal thumb_position x y second_arrange,half_width=if horizontal then thumb_length/2 else half_width,half_height=if horizontal then half_height else thumb_length/2})) vector_visual in Vector_visual {arrange=first_arrange,collect_order=DS.fromList [extension_slider_outer_rectangle_visual_index,extension_slider_inner_rectangle_visual_index,extension_slider_first_triangle_visual_base_index+get_store_widget (vector_widget DV.! extension_slider_first_triangle_state_index),extension_slider_second_triangle_visual_base_index+get_store_widget (vector_widget DV.! extension_slider_second_triangle_state_index),extension_slider_thumb_visual_base_index+thumb_state],vector_visual=new_vector_visual,size=size}
        _->EF.empty_error
    _->EF.empty_error

update_slider::ET.Has_call_stack=>Widget a b c d e->Maybe (Widget a b c d e)
update_slider=update_extension_widget extension_slider_dirty_index

page_getter::ET.Has_call_stack=>Widget a b c d e->(FCT.CFloat,FCT.CFloat,FCT.CFloat)
page_getter widget=case extract_extension_widget_vector widget DV.! extension_page_visual_index of
    Vector_visual {vector_visual}->case vector_visual DV.! extension_page_text_visual_index of
        Text {half_height,current_y,min_y,max_y}->(max_y-min_y+2*half_height,2*half_height,current_y-min_y)
        _->EF.empty_error
    _->EF.empty_error

page_setter::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Maybe (Widget a b c d e->Widget a b c d e)
page_setter cached_offset offset=if cached_offset==offset then Nothing else Just (page_setter_a offset)

page_setter_a::ET.Has_call_stack=>FCT.CFloat->Widget a b c d e->Widget a b c d e
page_setter_a offset=modify_extension_widget (page_setter_b offset)

page_setter_b::ET.Has_call_stack=>FCT.CFloat->Widget a b c d e->Widget a b c d e
page_setter_b offset widget=case widget of
    Vector {index,vector_widget}->Vector {index=index,vector_widget=CMST.runST (page_setter_c offset vector_widget)}
    _->EF.empty_error

page_setter_c::ET.Has_call_stack=>DVM.PrimMonad f=>FCT.CFloat->DV.Vector (Widget a b c d e)->f (DV.Vector (Widget a b c d e))
page_setter_c offset vector_widget=do
    new_vector_widget<-DV.thaw vector_widget
    DVM.write new_vector_widget extension_page_visual_index (page_setter_d offset (vector_widget DV.! extension_page_visual_index))
    DVM.write new_vector_widget extension_page_dirty_index (Store {store=convert True})
    DV.unsafeFreeze new_vector_widget

page_setter_d::ET.Has_call_stack=>FCT.CFloat->Widget a b c d e->Widget a b c d e
page_setter_d offset widget=case widget of
    Vector_visual {arrange,collect_order,vector_visual,size}->Vector_visual {arrange=arrange,collect_order=collect_order,vector_visual=CMST.runST (page_setter_e offset vector_visual),size=size}
    _->EF.empty_error

page_setter_e::ET.Has_call_stack=>DVM.PrimMonad f=>FCT.CFloat->DV.Vector Visual->f (DV.Vector Visual)
page_setter_e offset vector_visual=do
    new_vector_visual<-DV.thaw vector_visual
    DVM.write new_vector_visual extension_page_text_visual_index (page_setter_f offset (vector_visual DV.! extension_page_text_visual_index))
    DV.unsafeFreeze new_vector_visual

page_setter_f::ET.Has_call_stack=>FCT.CFloat->Visual->Visual
page_setter_f offset visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=min_y+offset,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked}
    _->EF.empty_error

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
{-# INLINE slider_widget_trigger #-}
{-# INLINE set_slider_updated #-}
{-# INLINE update_thumb_arrange #-}
{-# INLINE view_slider #-}
{-# INLINE view_slider_a #-}
{-# INLINE update_slider #-}
{-# INLINE page_getter #-}
{-# INLINE page_setter #-}
{-# INLINE page_setter_a #-}
{-# INLINE page_setter_b #-}
{-# INLINE page_setter_c #-}
{-# INLINE page_setter_d #-}
{-# INLINE page_setter_e #-}
{-# INLINE page_setter_f #-}