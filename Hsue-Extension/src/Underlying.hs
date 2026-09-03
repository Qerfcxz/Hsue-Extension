{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Underlying where

import Engine.Type
import qualified Error.Type as ET
import qualified Foreign.C.Types as FCT

above_box::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_box x y center_x center_y half_width half_height=abs (x-center_x)<=half_width&&abs (y-center_y)<=half_height

above_triangle::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_triangle x y center_x center_y radius=above_box x y center_x center_y radius radius

get_local_coordinate::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Arrange->(FCT.CFloat,FCT.CFloat)
get_local_coordinate click_x click_y arrange=case arrange of
    Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=click_x-point.x-matrix.x in let new_y=click_y-point.y-matrix.y in (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant,matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)

above_extension_box::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Arrange->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_extension_box click_x click_y arrange x y half_width half_height=let (local_x,local_y)=get_local_coordinate click_x click_y arrange in abs (local_x-x)<=half_width&&abs (local_y-y)<=half_height

above_extension_triangle::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Arrange->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_extension_triangle click_x click_y arrange x y radius=above_extension_box click_x click_y arrange x y radius radius

{-# INLINE above_box #-}
{-# INLINE above_triangle #-}
{-# INLINE get_local_coordinate #-}
{-# INLINE above_extension_box #-}
{-# INLINE above_extension_triangle #-}