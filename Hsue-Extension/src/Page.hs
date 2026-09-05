{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Page where

import Type
import Underlying
import Engine.Collector
import Engine.Helper
import Engine.Text
import Engine.Type
import Engine.Underlying
import Engine.Widget
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT

create_page_request::ET.Has_call_stack=>Custom_extension a=>(Event a->Engine a->Maybe Int)->Extension_visual_request a->Widget_request a
create_page_request next page_request=case page_request of
    Page_request {}->Visual_trigger_request {next=next,visual_trigger=page_visual_trigger,visual_request=Custom_visual_request {custom=page_request}}
    _->EF.empty_error

create_page_visual::ET.Has_call_stack=>Custom_extension a=>Extension_visual_request a->Engine a->IO (Engine a,Extension_visual a)
create_page_visual page_request engine=case page_request of
    Page_request {window_id,arrange,x,y,page_width,page_height,inner_thickness,outer_thickness,step_size,max_search_index,calculate_width,calculate_typesetting,anchor,article,load,color,outer_color,hovered_color,outer_hovered_color,selected_color,outer_selected_color,hovered_selected_color,outer_hovered_selected_color}->do
        (new_engine,text_visual)<-create_visual (Text_request {arrange=default_arrange {point=Point {x=x,y=y}},text_width=max 0 (page_width-2*inner_thickness),text_height=max 0 (page_height-2*inner_thickness),max_search_index=max_search_index,calculate_width=calculate_width,calculate_typesetting=calculate_typesetting,anchor=anchor,article=article,load=load}) engine
        return (new_engine,Page {window_id=window_id,arrange=arrange,x=x,y=y,half_width=page_width/2,half_height=page_height/2,inner_thickness=inner_thickness,outer_thickness=outer_thickness,step_size=step_size,dirty=True,hovered=False,pressed=False,color=color,outer_color=outer_color,hovered_color=hovered_color,outer_hovered_color=outer_hovered_color,selected_color=selected_color,outer_selected_color=outer_selected_color,hovered_selected_color=hovered_selected_color,outer_hovered_selected_color=outer_hovered_selected_color,text=text_visual})
    _->EF.empty_error

get_page_color::ET.Has_call_stack=>Bool->Bool->Color->Color->Color->Color->Color
get_page_color hovered pressed normal hovered_color selected_color hovered_selected_color=if pressed then if hovered then hovered_selected_color else selected_color else if hovered then hovered_color else normal

update_page_state::ET.Has_call_stack=>Bool->Bool->Bool->Bool->Visual a->Extension_visual a->Extension_visual a
update_page_state strict_match dirty hovered pressed text page=case page of
    Page {window_id,arrange,x,y,half_width,half_height,inner_thickness,outer_thickness,step_size,color,outer_color,hovered_color,outer_hovered_color,selected_color,outer_selected_color,hovered_selected_color,outer_hovered_selected_color}->Page {window_id=window_id,arrange=arrange,x=x,y=y,half_width=half_width,half_height=half_height,inner_thickness=inner_thickness,outer_thickness=outer_thickness,step_size=step_size,dirty=dirty,hovered=hovered,pressed=pressed,color=color,outer_color=outer_color,hovered_color=hovered_color,outer_hovered_color=outer_hovered_color,selected_color=selected_color,outer_selected_color=outer_selected_color,hovered_selected_color=hovered_selected_color,outer_hovered_selected_color=outer_hovered_selected_color,text=text}
    _->if strict_match then EF.empty_error else page

page_visual_trigger::ET.Has_call_stack=>Custom_extension a=>Event a->Engine a->Visual a->(Visual a,Engine a->Engine a)
page_visual_trigger event engine visual=case event of
    At {window_id=this_window_id,action}->case visual of
        Custom_visual {custom}->case custom of
            Page {window_id,arrange,x,y,half_width,half_height,step_size,hovered,pressed,text}->if this_window_id==window_id
                then case action of
                    Press {press,change}->case press of
                        Press_down->if pressed
                            then case change of
                                Key_down->(Custom_visual {custom=update_page_state engine.strict_match True hovered pressed (scroll_text engine.strict_match step_size text) custom},id)
                                Key_up->(Custom_visual {custom=update_page_state engine.strict_match True hovered pressed (scroll_text engine.strict_match (negate step_size) text) custom},id)
                                Key_page_down->(Custom_visual {custom=update_page_state engine.strict_match True hovered pressed (scroll_bottom_text engine.strict_match text) custom},id)
                                Key_page_up->(Custom_visual {custom=update_page_state engine.strict_match True hovered pressed (scroll_top_text engine.strict_match text) custom},id)
                                _->(visual,id)
                            else (visual,id)
                        _->(visual,id)
                    Click {press,mouse_button,x=click_x,y=click_y}->case mouse_button of
                        Mouse_button_left->case press of
                            Press_down->let above=above_extension_box click_x click_y arrange x y half_width half_height in if above/=pressed then (Custom_visual {custom=update_page_state engine.strict_match True hovered above text custom},id) else (visual,id)
                            _->(visual,id)
                        Mouse_button_right->case press of
                            Press_down->let above=above_extension_box click_x click_y arrange x y half_width half_height in if above&&pressed then (Custom_visual {custom=update_page_state engine.strict_match True hovered False text custom},id) else (visual,id)
                            _->(visual,id)
                        _->(visual,id)
                    Move {x=move_x,y=move_y}->let above=above_extension_box move_x move_y arrange x y half_width half_height in if above/=hovered then (Custom_visual {custom=update_page_state engine.strict_match True above pressed text custom},if above then \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}} else \this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (visual,id)
                    Scroll {x=scroll_x,y=scroll_y,delta_y}->if pressed&&above_extension_box scroll_x scroll_y arrange x y half_width half_height then (Custom_visual {custom=update_page_state engine.strict_match True hovered pressed (scroll_text engine.strict_match (negate delta_y*step_size) text) custom},id) else (visual,id)
                    _->(visual,id)
                else (visual,id)
            _->(visual,id)
        _->(visual,id)
    _->(visual,id)

collect_page_visual::ET.Has_call_stack=>Custom_extension a=>(Arrange->Arrange)->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Extension_visual a->DS.Seq (Submit a)
collect_page_visual transform u v maybe_border page=case page of
    Page {arrange,x,y,half_width,half_height,inner_thickness,outer_thickness,hovered,pressed,color,outer_color,hovered_color,outer_hovered_color,selected_color,outer_selected_color,hovered_selected_color,outer_hovered_selected_color,text}->case text of
        Text {arrange=text_arrange,current_y,anchor,article}->case transform arrange of
            Arrange {point=new_point,matrix=new_matrix,color=new_color}->case text_arrange of
                Arrange {matrix=text_matrix,color=text_color}->let base_point=Point {x=new_point.x+x,y=new_point.y+y} in DS.fromList [create_submit_rectangle Submit_default maybe_border (Arrange {point=base_point,matrix=new_matrix,color=multiply_color new_color (get_page_color hovered pressed outer_color outer_hovered_color outer_selected_color outer_hovered_selected_color)}) (half_width+outer_thickness) (half_height+outer_thickness) u v u v,create_submit_rectangle Submit_default maybe_border (Arrange {point=base_point,matrix=new_matrix,color=multiply_color new_color (get_page_color hovered pressed color hovered_color selected_color hovered_selected_color)}) half_width half_height u v u v,create_submit_text Submit_default maybe_border (Arrange {point=base_point,matrix=multiply_matrix new_matrix text_matrix,color=multiply_color new_color text_color}) (half_width-inner_thickness) (half_height-inner_thickness) current_y anchor article]
        _->EF.empty_error
    _->EF.empty_error

page_getter::ET.Has_call_stack=>Custom_extension a=>Widget a->(FCT.CFloat,FCT.CFloat,FCT.CFloat)
page_getter widget=case widget of
    Visual_trigger {visual}->case visual of
        Custom_visual {custom}->case custom of
            Page {text}->case text of
                Text {half_height,current_y,min_y,max_y}->(max_y-min_y+2*half_height,2*half_height,current_y-min_y)
                _->EF.empty_error
            _->EF.empty_error
        _->EF.empty_error
    _->EF.empty_error

page_setter::ET.Has_call_stack=>Custom_extension a=>Bool->FCT.CFloat->FCT.CFloat->Maybe (Widget a->Widget a)
page_setter strict_match cached_offset offset=if cached_offset==offset then Nothing else Just (page_setter_a strict_match offset)

page_setter_a::ET.Has_call_stack=>Custom_extension a=>Bool->FCT.CFloat->Widget a->Widget a
page_setter_a strict_match offset widget=case widget of
    Visual_trigger {next,visual_trigger,visual}->case visual of
        Custom_visual {custom}->case custom of
            Page {window_id,arrange,x,y,half_width,half_height,inner_thickness,outer_thickness,step_size,hovered,pressed,color,outer_color,hovered_color,outer_hovered_color,selected_color,outer_selected_color,hovered_selected_color,outer_hovered_selected_color,text}->case text of
                Text {arrange=text_arrange,half_width=text_half_width,half_height=text_half_height,min_y,max_y,anchor,article,charset,locked}->Visual_trigger {next=next,visual_trigger=visual_trigger,visual=Custom_visual {custom=Page {window_id=window_id,arrange=arrange,x=x,y=y,half_width=half_width,half_height=half_height,inner_thickness=inner_thickness,outer_thickness=outer_thickness,step_size=step_size,dirty=True,hovered=hovered,pressed=pressed,color=color,outer_color=outer_color,hovered_color=hovered_color,outer_hovered_color=outer_hovered_color,selected_color=selected_color,outer_selected_color=outer_selected_color,hovered_selected_color=hovered_selected_color,outer_hovered_selected_color=outer_hovered_selected_color,text=Text {arrange=text_arrange,half_width=text_half_width,half_height=text_half_height,current_y=min_y+offset,min_y=min_y,max_y=max_y,anchor=anchor,article=article,charset=charset,locked=locked}}}}
                _->if strict_match then EF.empty_error else widget
            _->if strict_match then EF.empty_error else widget
        _->if strict_match then EF.empty_error else widget
    _->if strict_match then EF.empty_error else widget

{-# INLINE create_page_request #-}
{-# INLINE create_page_visual #-}
{-# INLINE get_page_color #-}
{-# INLINE update_page_state #-}
{-# INLINE page_visual_trigger #-}
{-# INLINE collect_page_visual #-}
{-# INLINE page_getter #-}
{-# INLINE page_setter #-}
{-# INLINE page_setter_a #-}