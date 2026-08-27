{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Common where

import Engine.Helper
import Engine.Operation
import Engine.Type
import Engine.Underlying
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT

above_box::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_box point_x point_y center_x center_y half_width half_height=abs (point_x-center_x)<=half_width&&abs (point_y-center_y)<=half_height

above_triangle::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_triangle x y center_x center_y radius=above_box x y center_x center_y radius radius

above_extension_widget::ET.Has_call_stack=>Int->FCT.CFloat->FCT.CFloat->Widget a->Bool
above_extension_widget index x y widget=case widget of
    Vector_visual {arrange=first_arrange,vector_visual}->case vector_visual DV.! index of
        Rectangle {arrange=second_arrange,half_width,half_height}->case combine_arrange first_arrange second_arrange of
            Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-point.x-matrix.x in let new_y=y-point.y-matrix.y in abs (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=half_width&&abs (matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=half_height
        _->EF.empty_error
    _->EF.empty_error

create_origin_rectangle_request::ET.Has_call_stack=>Color->FCT.CFloat->FCT.CFloat->Visual_request a
create_origin_rectangle_request color half_width half_height=Rectangle_request {arrange=Arrange {point=origin_point,matrix=identity_matrix,color=color},rectangle_width=2*half_width,rectangle_height=2*half_height}

create_rectangle_request::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Color->FCT.CFloat->FCT.CFloat->Visual_request a
create_rectangle_request x y color half_width half_height=Rectangle_request {arrange=Arrange {point=Point {x=x,y=y},matrix=identity_matrix,color=color},rectangle_width=2*half_width,rectangle_height=2*half_height}

create_triangle_request::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Color->Point->Point->Point->Visual_request a
create_triangle_request x y color first_point second_point third_point=Triangle_request {arrange=Arrange {point=Point {x=x,y=y},matrix=identity_matrix,color=color},first_point=first_point,second_point=second_point,third_point=third_point}

extract_extension_widget_vector::ET.Has_call_stack=>Widget a->DV.Vector (Widget a)
extract_extension_widget_vector this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->vector_widget
        _->EF.empty_error
    _->EF.empty_error

modify_extension_widget::ET.Has_call_stack=>(Widget a->Widget a)->Widget a->Widget a
modify_extension_widget modify this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=modify widget}
    _->EF.empty_error

view_extension_widget::ET.Has_call_stack=>(DV.Vector (Widget a)->Widget a)->Widget a->Widget a
view_extension_widget view_vector this_widget=view_vector (extract_extension_widget_vector this_widget)

update_extension_widget::ET.Has_call_stack=>Int->Widget a->Maybe (Widget a)
update_extension_widget dirty_index this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->if get_store_widget (vector_widget DV.! dirty_index) then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_bool (const False) dirty_index widget}) else Nothing
        _->EF.empty_error
    _->EF.empty_error

view_vector_bool::ET.Has_call_stack=>Widget a->Int->Bool
view_vector_bool widget index=case widget of
    Vector {vector_widget}->case vector_widget DV.! index of
        Store {store}->convert store
        _->EF.empty_error
    _->EF.empty_error

update_vector_bool::ET.Has_call_stack=>(Bool->Bool)->Int->Widget a->Widget a
update_vector_bool update index=update_vector_widget index (update_store_widget update)

view_extension_visual::ET.Has_call_stack=>Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->DV.Vector (Widget a)->Widget a
view_extension_visual visual_index hovered_index state_index state_hovered_offset state_unhovered_offset normal_hovered_offset normal_unhovered_offset outer_base inner_base content_index vector_widget=case vector_widget DV.! visual_index of
    Vector_visual {arrange,vector_visual,size}->let hovered=get_store_widget (vector_widget DV.! hovered_index) in let offset=if get_store_widget (vector_widget DV.! state_index) then if hovered then state_hovered_offset else state_unhovered_offset else if hovered then normal_hovered_offset else normal_unhovered_offset in Vector_visual {arrange=arrange,collect_order=(outer_base+offset) DS.<| (inner_base+offset) DS.<| DS.singleton content_index,vector_visual=vector_visual,size=size}
    _->EF.empty_error

{-# INLINE above_box #-}
{-# INLINE above_triangle #-}
{-# INLINE above_extension_widget #-}
{-# INLINE create_origin_rectangle_request #-}
{-# INLINE create_rectangle_request #-}
{-# INLINE create_triangle_request #-}
{-# INLINE extract_extension_widget_vector #-}
{-# INLINE modify_extension_widget #-}
{-# INLINE view_extension_widget #-}
{-# INLINE update_extension_widget #-}
{-# INLINE view_vector_bool #-}
{-# INLINE update_vector_bool #-}
{-# INLINE view_extension_visual #-}