{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Instance where

import Button
import Page
import Slider
import Type
import Engine.Type
import qualified Error.Type as ET
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP

instance Custom_extension a=>Custom a where
    type Custom_state a=Extension_state a
    type Custom_event a=Extension_event a
    type Custom_visual a=Extension_visual a
    type Custom_visual_request a=Extension_visual_request a
    type Custom_submit_data a=Extension_submit_data a
    custom_visual_collect=extension_visual_collect
    custom_visual_remove=extension_visual_remove
    custom_visual_unlock=extension_visual_unlock
    custom_visual_lock=extension_visual_lock
    custom_visual_request=extension_visual_request
    custom_submit_data=extension_submit_data

extension_visual_collect::ET.Has_call_stack=>Custom_extension a=>(Arrange->Arrange)->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Extension_visual a->DS.Seq (Submit a)
extension_visual_collect transform u v maybe_border visual=case visual of
    Page {}->collect_page_visual transform u v maybe_border visual
    Button {}->collect_button_visual transform u v maybe_border visual
    Slider {}->collect_slider_visual transform u v maybe_border visual
    Custom_extension_visual {custom}->extension_custom_visual_collect transform u v maybe_border custom

extension_visual_remove::ET.Has_call_stack=>Custom_extension a=>Extension_visual a->Engine a->IO (Engine a)
extension_visual_remove visual engine=case visual of
    Custom_extension_visual {custom}->extension_custom_visual_remove custom engine
    _->return engine

extension_visual_unlock::ET.Has_call_stack=>Custom_extension a=>Extension_visual a->Engine a->IO (Engine a,Extension_visual a)
extension_visual_unlock visual engine=case visual of
    Custom_extension_visual {custom}->do
        (new_engine,new_custom)<-extension_custom_visual_unlock custom engine
        return (new_engine,Custom_extension_visual {custom=new_custom})
    _->return (engine,visual)

extension_visual_lock::ET.Has_call_stack=>Custom_extension a=>Extension_visual a->Extension_visual a
extension_visual_lock visual=case visual of
    Custom_extension_visual {custom}->Custom_extension_visual {custom=extension_custom_visual_lock custom}
    _->visual

extension_visual_request::ET.Has_call_stack=>Custom_extension a=>Extension_visual_request a->Engine a->IO (Engine a,Extension_visual a)
extension_visual_request visual_request engine=case visual_request of
    Page_request {}->create_page_visual visual_request engine
    Button_request {}->create_button_visual visual_request engine
    Slider_request {}->create_slider_visual visual_request engine
    Custom_extension_visual_request {custom}->do
        (new_engine,new_custom)<-extension_custom_visual_request custom engine
        return (new_engine,Custom_extension_visual {custom=new_custom})

extension_submit_data::ET.Has_call_stack=>Custom_extension a=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->Extension_submit_data a->IO ()
extension_submit_data vertex_ptr index_ptr vertex_index parameter_index submit_data=case submit_data of
    Custom_extension_submit_data {custom}->extension_custom_submit_data vertex_ptr index_ptr vertex_index parameter_index custom

{-# INLINE extension_visual_collect #-}
{-# INLINE extension_visual_remove #-}
{-# INLINE extension_visual_unlock #-}
{-# INLINE extension_visual_lock #-}
{-# INLINE extension_visual_request #-}
{-# INLINE extension_submit_data #-}