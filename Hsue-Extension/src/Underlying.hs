{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Underlying where

import Engine.Helper
import Engine.Type
import qualified Error.Type as ET
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.Map as DM
import qualified Foreign.C.Types as FCT

to_key_setting::ET.Has_call_stack=>DM.Map (DHS.HashSet Key) a->DHMS.HashMap Integer a
to_key_setting=DM.foldlWithKey' (\key_setting key value->insert_foldable_enumeration key value key_setting) DHMS.empty

above_box::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_box x y center_x center_y half_width half_height=abs (x-center_x)<=half_width&&abs (y-center_y)<=half_height

get_local_coordinate::ET.Has_call_stack=>Arrange->FCT.CFloat->FCT.CFloat->(FCT.CFloat,FCT.CFloat)
get_local_coordinate arrange click_x click_y=case arrange of
    Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=click_x-point.x-matrix.x in let new_y=click_y-point.y-matrix.y in (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant,matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)

above_extension_box::ET.Has_call_stack=>Arrange->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Bool
above_extension_box arrange click_x click_y x y half_width half_height=let (local_x,local_y)=get_local_coordinate arrange click_x click_y in above_box local_x local_y x y half_width half_height

{-# INLINE to_key_setting #-}
{-# INLINE above_box #-}
{-# INLINE get_local_coordinate #-}
{-# INLINE above_extension_box #-}