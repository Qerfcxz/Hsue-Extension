{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Common where

import Type
import Engine.Helper
import Engine.Type
import qualified Foreign.C.Types as FCT

above_box::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_box point_x point_y center_x center_y half_width half_height=abs (point_x-center_x)<=half_width&&abs (point_y-center_y)<=half_height

create_origin_rectangle_request::Color->FCT.CFloat->FCT.CFloat->Visual_request
create_origin_rectangle_request color half_width half_height=case color of
    Color {red,green,blue,alpha}->Rectangle_request {arrange=Arrange {point=origin,matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},rectangle_width=2*half_width,rectangle_height=2*half_height}

create_rectangle_request::FCT.CFloat->FCT.CFloat->Color->FCT.CFloat->FCT.CFloat->Visual_request
create_rectangle_request x y color half_width half_height=case color of
    Color {red,green,blue,alpha}->Rectangle_request {arrange=Arrange {point=Point {x=x,y=y},matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},rectangle_width=2*half_width,rectangle_height=2*half_height}

create_triangle_request_with_pos::FCT.CFloat->FCT.CFloat->Color->Point->Point->Point->Visual_request
create_triangle_request_with_pos x y color first_point second_point third_point=case color of
    Color {red,green,blue,alpha}->Triangle_request {arrange=Arrange {point=Point {x=x,y=y},matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},first_point=first_point,second_point=second_point,third_point=third_point}

{-# INLINE above_box #-}
{-# INLINE create_origin_rectangle_request #-}
{-# INLINE create_rectangle_request #-}
{-# INLINE create_triangle_request_with_pos #-}