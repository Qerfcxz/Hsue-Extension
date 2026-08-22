{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Button where

import Common
import Engine.Operation
import Engine.Type
import qualified Error.Error as EE
import qualified Data.Sequence as DS
import qualified Data.Vector as DV

extension_button_visual_index::Int
extension_button_visual_index=0

extension_button_hovered_index::Int
extension_button_hovered_index=1

extension_button_pressed_index::Int
extension_button_pressed_index=2

extension_button_dirty_index::Int
extension_button_dirty_index=3

extension_button_window_id_index::Int
extension_button_window_id_index=4

extension_button_content_visual_index::Int
extension_button_content_visual_index=0

extension_button_inner_rectangle_index::Int
extension_button_inner_rectangle_index=1

extension_button_root_vector_index::Int
extension_button_root_vector_index=0

extension_button_unpressed_unhovered_offset::Int
extension_button_unpressed_unhovered_offset=0

extension_button_unpressed_hovered_offset::Int
extension_button_unpressed_hovered_offset=2

extension_button_pressed_unhovered_offset::Int
extension_button_pressed_unhovered_offset=4

extension_button_pressed_hovered_offset::Int
extension_button_pressed_hovered_offset=6

extension_button_inner_rectangle_base_offset::Int
extension_button_inner_rectangle_base_offset=1

extension_button_outer_rectangle_base_offset::Int
extension_button_outer_rectangle_base_offset=2

button_widget_trigger::(Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)
button_widget_trigger this_action event _ widget=case event of
    At {window_id,action}->case widget of
        Vector {vector_widget}->if window_id==get_store_widget (vector_widget DV.! extension_button_window_id_index)
            then case action of
                Click {press,mouse_button,x,y}->case mouse_button of
                    Mouse_button_left->case press of
                        Press_down->if above_extension_widget extension_button_inner_rectangle_index x y (vector_widget DV.! extension_button_visual_index) then (update_vector_bool (const True) extension_button_dirty_index (update_vector_bool (const True) extension_button_pressed_index widget),id) else (widget,id)
                        Press_up->if view_vector_bool widget extension_button_pressed_index then let new_widget=update_vector_bool (const True) extension_button_dirty_index (update_vector_bool (const False) extension_button_pressed_index widget) in if above_extension_widget extension_button_inner_rectangle_index x y (vector_widget DV.! extension_button_visual_index) then (new_widget,this_action) else (new_widget,id) else (widget,id)
                    _->(widget,id)
                Move {x,y}->let above=above_extension_widget extension_button_inner_rectangle_index x y (vector_widget DV.! extension_button_visual_index) in if above/=view_vector_bool widget extension_button_hovered_index then (update_vector_bool (const True) extension_button_dirty_index (update_vector_bool (const above) extension_button_hovered_index widget),if above then \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (widget,id)
                _->(widget,id)
            else (widget,id)
        _->EE.empty_error
    _->(widget,id)

view_button::Widget a b c d e->Widget a b c d e
view_button=view_extension_widget (view_extension_visual extension_button_visual_index extension_button_hovered_index extension_button_pressed_index extension_button_pressed_hovered_offset extension_button_pressed_unhovered_offset extension_button_unpressed_hovered_offset extension_button_unpressed_unhovered_offset extension_button_outer_rectangle_base_offset extension_button_inner_rectangle_base_offset extension_button_content_visual_index)

update_button::Widget a b c d e->Maybe (Widget a b c d e)
update_button=update_extension_widget extension_button_dirty_index

{-# INLINE extension_button_visual_index #-}
{-# INLINE extension_button_hovered_index #-}
{-# INLINE extension_button_pressed_index #-}
{-# INLINE extension_button_dirty_index #-}
{-# INLINE extension_button_window_id_index #-}
{-# INLINE extension_button_content_visual_index #-}
{-# INLINE extension_button_inner_rectangle_index #-}
{-# INLINE extension_button_root_vector_index #-}
{-# INLINE extension_button_unpressed_unhovered_offset #-}
{-# INLINE extension_button_unpressed_hovered_offset #-}
{-# INLINE extension_button_pressed_unhovered_offset #-}
{-# INLINE extension_button_pressed_hovered_offset #-}
{-# INLINE extension_button_inner_rectangle_base_offset #-}
{-# INLINE extension_button_outer_rectangle_base_offset #-}
{-# INLINE button_widget_trigger #-}
{-# INLINE view_button #-}
{-# INLINE update_button #-}