{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Common where

import Type
import Engine.Helper
import Engine.Operation
import Engine.Type
import Engine.Underlying
import qualified Error.Error as EE
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT

above_box::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_box point_x point_y center_x center_y half_width half_height=abs (point_x-center_x)<=half_width&&abs (point_y-center_y)<=half_height

above_triangle::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_triangle x y center_x center_y radius=above_box x y center_x center_y radius radius

above_extension_widget::Int->FCT.CFloat->FCT.CFloat->Widget a b c d e->Bool
above_extension_widget index x y widget=case widget of
    Vector_visual {arrange=first_arrange,vector_visual}->case vector_visual DV.! index of
        Rectangle {arrange=second_arrange,half_width,half_height}->case combine_arrange first_arrange second_arrange of
            Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-point.x-matrix.x in let new_y=y-point.y-matrix.y in abs (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=half_width&&abs (matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=half_height
        _->EE.quick_error "above_extension_widget" 0
    _->EE.quick_error "above_extension_widget" 1

create_origin_rectangle_request::Color->FCT.CFloat->FCT.CFloat->Visual_request
create_origin_rectangle_request color half_width half_height=case color of
    Color {red,green,blue,alpha}->Rectangle_request {arrange=Arrange {point=origin,matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},rectangle_width=2*half_width,rectangle_height=2*half_height}

create_rectangle_request::FCT.CFloat->FCT.CFloat->Color->FCT.CFloat->FCT.CFloat->Visual_request
create_rectangle_request x y color half_width half_height=case color of
    Color {red,green,blue,alpha}->Rectangle_request {arrange=Arrange {point=Point {x=x,y=y},matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},rectangle_width=2*half_width,rectangle_height=2*half_height}

create_triangle_request::FCT.CFloat->FCT.CFloat->Color->Point->Point->Point->Visual_request
create_triangle_request x y color first_point second_point third_point=case color of
    Color {red,green,blue,alpha}->Triangle_request {arrange=Arrange {point=Point {x=x,y=y},matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},first_point=first_point,second_point=second_point,third_point=third_point}

extract_extension_widget_vector::Widget a b c d e->DV.Vector (Widget a b c d e)
extract_extension_widget_vector this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->vector_widget
        _->EE.quick_error "extract_extension_widget_vector" 0
    _->EE.quick_error "extract_extension_widget_vector" 1

modify_extension_widget::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
modify_extension_widget modify this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->Widget_trigger {next=next,widget_trigger=widget_trigger,widget=modify widget}
    _->EE.quick_error "modify_extension_widget" 0

view_extension_widget::(DV.Vector (Widget a b c d e)->Widget a b c d e)->Widget a b c d e->Widget a b c d e
view_extension_widget view_vector this_widget=view_vector (extract_extension_widget_vector this_widget)

update_extension_widget::Int->Widget a b c d e->Maybe (Widget a b c d e)
update_extension_widget dirty_index this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->if get_store_widget (vector_widget DV.! dirty_index) then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_bool (const False) dirty_index widget}) else Nothing
        _->EE.quick_error "update_extension_widget" 0
    _->EE.quick_error "update_extension_widget" 1

view_vector_bool::Widget a b c d e->Int->Bool
view_vector_bool widget index=case widget of
    Vector {vector_widget}->case vector_widget DV.! index of
        Store {store}->convert store
        _->EE.quick_error "view_vector_bool" 0
    _->EE.quick_error "view_vector_bool" 1

update_vector_bool::(Bool->Bool)->Int->Widget a b c d e->Widget a b c d e
update_vector_bool update index=update_vector_widget index (update_store_widget update)

view_extension_visual::Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->DV.Vector (Widget a b c d e)->Widget a b c d e
view_extension_visual visual_index hovered_index state_index state_hovered_offset state_unhovered_offset normal_hovered_offset normal_unhovered_offset outer_base inner_base content_index vector_widget=case vector_widget DV.! visual_index of
    Vector_visual {arrange,size,vector_visual}->let hovered=get_store_widget (vector_widget DV.! hovered_index) in let offset=if get_store_widget (vector_widget DV.! state_index) then if hovered then state_hovered_offset else state_unhovered_offset else if hovered then normal_hovered_offset else normal_unhovered_offset in Vector_visual {arrange=arrange,collect_order=(outer_base+offset) DS.<| (inner_base+offset) DS.<| DS.singleton content_index,size=size,vector_visual=vector_visual}
    _->EE.quick_error "view_extension_visual" 0

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