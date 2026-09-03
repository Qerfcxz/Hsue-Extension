{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Button where

import Underlying
import Type
import Engine.Collector
import Engine.Helper
import Engine.Type
import Engine.Underlying
import Engine.Widget
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT

create_button_request::ET.Has_call_stack=>Custom_extension a=>(Event a->Engine a->Maybe Int)->(Engine a->Engine a)->Extension_visual_request a->Widget_request a
create_button_request next action button_request=case button_request of
    Button_request {}->Visual_trigger_request {next=next,visual_trigger=button_visual_trigger action,visual_request=Custom_visual_request {custom=button_request}}
    _->EF.empty_error

create_button_visual::ET.Has_call_stack=>Custom_extension a=>Extension_visual_request a->Engine a->IO (Engine a,Extension_visual a)
create_button_visual button_request engine=case button_request of
    Button_request {window_id,arrange,x,y,button_width,button_height,inner_thickness,outer_thickness,anchor,article,load,color,outer_color,hovered_color,outer_hovered_color,pressed_color,outer_pressed_color,hovered_pressed_color,outer_hovered_pressed_color}->let half_width=button_width/2 in let half_height=button_height/2 in let text_width=max 0 (button_width-2*inner_thickness) in let text_height=max 0 (button_height-2*inner_thickness) in do
        (new_engine,text_visual)<-create_visual (Text_request {arrange=default_arrange {point=Point {x=x,y=y}},text_width=text_width,text_height=text_height,calculate_width=const (const (const (1/0))),calculate_typesetting=button_calculate_typesetting text_height,anchor=anchor,article=article,load=load}) engine
        return (new_engine,Button {window_id=window_id,arrange=arrange,x=x,y=y,half_width=half_width,half_height=half_height,inner_thickness=inner_thickness,outer_thickness=outer_thickness,dirty=True,hovered=False,pressed=False,color=color,outer_color=outer_color,hovered_color=hovered_color,outer_hovered_color=outer_hovered_color,pressed_color=pressed_color,outer_pressed_color=outer_pressed_color,hovered_pressed_color=hovered_pressed_color,outer_hovered_pressed_color=outer_hovered_pressed_color,text=text_visual})
    _->EF.empty_error

button_calculate_typesetting::ET.Has_call_stack=>FCT.CFloat->DS.Seq (DS.Seq Row)->Int->Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat)
button_calculate_typesetting height article number index=if number==0||number<=index
    then (0,0,0)
    else case article of
        (row DS.:<| _) DS.:<| _->let half_height=height/2 in let offset=(row.max_up+row.min_down)/2 in (half_height-offset,half_height+offset,0)
        _->let half_height=height/2 in (half_height,half_height,0)

get_button_color::ET.Has_call_stack=>Bool->Bool->Color->Color->Color->Color->Color
get_button_color hovered pressed normal hovered_color pressed_color hovered_pressed_color=if pressed then if hovered then hovered_pressed_color else pressed_color else if hovered then hovered_color else normal

update_button_state::ET.Has_call_stack=>Bool->Bool->Bool->Extension_visual a->Extension_visual a
update_button_state dirty hovered pressed button=case button of
    Button {window_id,arrange,x,y,half_width,half_height,inner_thickness,outer_thickness,color,outer_color,hovered_color,outer_hovered_color,pressed_color,outer_pressed_color,hovered_pressed_color,outer_hovered_pressed_color,text}->Button {window_id=window_id,arrange=arrange,x=x,y=y,half_width=half_width,half_height=half_height,inner_thickness=inner_thickness,outer_thickness=outer_thickness,dirty=dirty,hovered=hovered,pressed=pressed,color=color,outer_color=outer_color,hovered_color=hovered_color,outer_hovered_color=outer_hovered_color,pressed_color=pressed_color,outer_pressed_color=outer_pressed_color,hovered_pressed_color=hovered_pressed_color,outer_hovered_pressed_color=outer_hovered_pressed_color,text=text}
    _->EF.empty_error

button_visual_trigger::ET.Has_call_stack=>Custom_extension a=>(Engine a->Engine a)->Event a->Engine a->Visual a->(Visual a,Engine a->Engine a)
button_visual_trigger this_action event _ visual=case event of
    At {window_id=this_window_id,action}->case visual of
        Custom_visual {custom}->case custom of
            Button {window_id,arrange,x,y,half_width,half_height,hovered,pressed}->if this_window_id==window_id
                then case action of
                    Click {press,mouse_button,x=click_x,y=click_y}->case mouse_button of
                        Mouse_button_left->case press of
                            Press_down->let above=above_extension_box click_x click_y arrange x y half_width half_height in if above then (Custom_visual {custom=update_button_state True hovered True custom},id) else (visual,id)
                            Press_up->if pressed then let new_button=update_button_state True hovered False custom in if above_extension_box click_x click_y arrange x y half_width half_height then (Custom_visual {custom=new_button},this_action) else (Custom_visual {custom=new_button},id) else (visual,id)
                        _->(visual,id)
                    Move {x=move_x,y=move_y}->let above=above_extension_box move_x move_y arrange x y half_width half_height in if above/=hovered then (Custom_visual {custom=update_button_state True above pressed custom},if above then \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (visual,id)
                    _->(visual,id)
                else (visual,id)
            _->(visual,id)
        _->(visual,id)
    _->(visual,id)

collect_button_visual::ET.Has_call_stack=>Custom_extension a=>(Arrange->Arrange)->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Extension_visual a->DS.Seq (Submit a)
collect_button_visual transform u v maybe_border button=case button of
    Button {arrange,x,y,half_width,half_height,inner_thickness,outer_thickness,hovered,pressed,color,outer_color,hovered_color,outer_hovered_color,pressed_color,outer_pressed_color,hovered_pressed_color,outer_hovered_pressed_color,text}->case text of
        Text {arrange=text_arrange,current_y,anchor,article}->case transform arrange of
            Arrange {point=new_point,matrix=new_matrix,color=new_color}->case text_arrange of
                Arrange {matrix=text_matrix,color=text_color}->let current_color=get_button_color hovered pressed color hovered_color pressed_color hovered_pressed_color in
                    let current_outer_color=get_button_color hovered pressed outer_color outer_hovered_color outer_pressed_color outer_hovered_pressed_color in
                    let outer_half_width=half_width+outer_thickness in
                    let outer_half_height=half_height+outer_thickness in
                    let center_x=new_point.x+x in
                    let center_y=new_point.y+y in
                    let base_point=Point {x=center_x,y=center_y} in
                    let outer_arrange=Arrange {point=base_point,matrix=new_matrix,color=multiply_color new_color current_outer_color} in
                    let inner_arrange=Arrange {point=base_point,matrix=new_matrix,color=multiply_color new_color current_color} in
                    let text_combined_arrange=Arrange {point=base_point,matrix=multiply_matrix new_matrix text_matrix,color=multiply_color new_color text_color} in
                    let outer_submit=create_submit_rectangle Submit_default maybe_border outer_arrange outer_half_width outer_half_height u v u v in
                    let inner_submit=create_submit_rectangle Submit_default maybe_border inner_arrange half_width half_height u v u v in
                    let text_submit=create_submit_text Submit_default maybe_border text_combined_arrange (half_width-inner_thickness) (half_height-inner_thickness) current_y anchor article in
                    DS.fromList [outer_submit,inner_submit,text_submit]
        _->EF.empty_error
    _->EF.empty_error

{-# INLINE create_button_request #-}
{-# INLINE create_button_visual #-}
{-# INLINE button_calculate_typesetting #-}
{-# INLINE get_button_color #-}
{-# INLINE update_button_state #-}
{-# INLINE button_visual_trigger #-}
{-# INLINE collect_button_visual #-}