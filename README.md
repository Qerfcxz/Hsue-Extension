# Hsue-Extension 🧩

**[English](#english) | [中文](#中文)**

---

<a id="english"></a>
# Hsue-Extension (English)

> High-level UI controls and integrated widget component suite designed for the [Hsue](https://github.com/Qerfcxz/Hsue) purely functional 2D UI engine.

⚠️ **EARLY DEVELOPMENT NOTICE** ⚠️
> **Hsue-Extension is currently in its very early stage.** Only a few foundational controls are implemented.
> **Roadmap:** Comprehensive development of integrated controls will officially begin after the single-font editable text box (`Editor`) widget is completed in the main [Hsue](https://github.com/Qerfcxz/Hsue) engine.

## 🚀 Overview

`Hsue-Extension` builds on top of `Hsue`'s core primitive framework (`Widget`, `Visual`, `Trigger`, `Selector`), encapsulating low-level layout and event management into stateful, rich, and interactive UI controls.

## 🧩 Current Integrated Controls

*   **Button (`Button.hs`)**: Multi-state interactive button with customizable inner/outer borders and dynamic colors (`normal`, `hovered`, `pressed`, `hovered_pressed`), automatic system cursor feedback, and clean state-change tracking.
*   **Page (`Page.hs`)**: Scrollable document & text container supporting smooth scrolling via mouse wheel, drag, and keyboard navigation (`Up`, `Down`, `Page Up`, `Page Down`), with integrated viewport bounds & clipping.
*   **Slider (`Slider.hs`)**: Interactive scrollbar / slider with bi-directional state binding. Supports custom tracks, thumbs, stepper triangles, drag-and-drop mechanics, and page-step jumps. Fully composable with `Page` for seamless content scrolling.

---

<a id="中文"></a>
# Hsue-Extension (中文)

> 专为 [Hsue](https://github.com/Qerfcxz/Hsue) 纯函数式 2D UI 引擎打造的高阶 UI 控件与集成组件库。

⚠️ **早期开发阶段提示** ⚠️
> **Hsue-Extension 目前仍处于极早期阶段。** 现阶段仅实现了少量基础控件。
> **后续规划：** 当主引擎 [Hsue](https://github.com/Qerfcxz/Hsue) 完成**单字体可编辑文本框 (`Editor`)** 控件的实现后，本项目将正式开始全面编写和丰富高阶集成控件库。

## 🚀 简介

`Hsue-Extension` 建立在 `Hsue` 底层的原元框架（如 `Widget`, `Visual`, `Trigger`, `Selector` 等）之上，将复杂的几何计算、状态记录与事件响应封装为开箱即用的高级交互 UI 控件。

## 🧩 当前内置控件

*   **按钮 (Button - `Button.hs`)**: 多状态交互按钮，支持内外边框与多种颜色状态（普通、悬停、按下、悬停按下）定制，具备鼠标悬停光标自动切换与高效的脏状态（Dirty Check）刷新机制。
*   **滚动页面 (Page - `Page.hs`)**: 文本与内容滚动容器，支持鼠标滚轮、拖拽以及键盘快捷键（`Up`, `Down`, `Page Up`, `Page Down`）翻拉，内置视口边界计算与裁剪。
*   **滑块 / 滚动条 (Slider - `Slider.hs`)**: 支持双向状态绑定的交互式滑块/滚动条。内置轨道、滑块（Thumb）及两端步进三角按钮，支持拖拽定位与整页点击跳转，可与 `Page` 容器无缝联动实现内容滚动。
