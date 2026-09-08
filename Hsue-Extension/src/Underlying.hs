{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Underlying where

import Engine.Type
import Engine.Helper
import qualified Error.Type as ET
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.Map as DM
import qualified Foreign.C.Types as FCT

to_key_setting::ET.Has_call_stack=>DM.Map (DHS.HashSet Key) a->DHMS.HashMap Integer a
to_key_setting=DM.foldlWithKey' (\key_setting key value->insert_foldable_enumeration key value key_setting) DHMS.empty

above_box::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_box x y center_x center_y half_width half_height=abs (x-center_x)<=half_width&&abs (y-center_y)<=half_height

above_first_triangle::ET.Has_call_stack=>Bool->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_first_triangle horizontal x y center_x center_y radius=let delta_x=x-center_x in let delta_y=y-center_y in if horizontal then delta_x>=negate radius&&delta_x<=radius&&abs delta_y<=(delta_x+radius)/2 else delta_y>=negate radius&&delta_y<=radius&&abs delta_x<=(radius-delta_y)/2

above_second_triangle::ET.Has_call_stack=>Bool->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_second_triangle horizontal x y center_x center_y radius=let delta_x=x-center_x in let delta_y=y-center_y in if horizontal then delta_x>=negate radius&&delta_x<=radius&&abs delta_y<=(radius-delta_x)/2 else delta_y>=negate radius&&delta_y<=radius&&abs delta_x<=(delta_y+radius)/2

above_triangle::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_triangle x y center_x center_y radius=above_box x y center_x center_y radius radius

get_local_coordinate::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Arrange->(FCT.CFloat,FCT.CFloat)
get_local_coordinate click_x click_y arrange=case arrange of
    Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=click_x-point.x-matrix.x in let new_y=click_y-point.y-matrix.y in (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant,matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)

above_extension_box::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Arrange->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_extension_box click_x click_y arrange x y half_width half_height=let (local_x,local_y)=get_local_coordinate click_x click_y arrange in above_box local_x local_y x y half_width half_height

above_extension_triangle::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Arrange->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_extension_triangle click_x click_y arrange x y radius=let (local_x,local_y)=get_local_coordinate click_x click_y arrange in above_triangle local_x local_y x y radius

{-# INLINE above_box #-}
{-# INLINE above_first_triangle #-}
{-# INLINE above_second_triangle #-}
{-# INLINE above_triangle #-}
{-# INLINE get_local_coordinate #-}
{-# INLINE above_extension_box #-}
{-# INLINE above_extension_triangle #-}