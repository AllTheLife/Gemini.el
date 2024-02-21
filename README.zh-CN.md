[English Version](./README.md) | 简体中文

### Gemini:
# Gemini.el
享受在 Emacs 中使用 Google Gemini 的快乐吧😀

## 依赖项
1. 拥有 Google 账号并拥有访问 Gemini 的权限

## 安装

本 package 将不会上传至melpa，谢谢理解

1. 克隆本仓库 `git clone https://github.com/AllTheLife/Gemini.el <path-to-site-lisp>`
2. 在emacs中加载本package
``` lisp
(add-to-list 'load-path "<path-to-gemini>")
(require 'gemini)
```
3. 你需要准备好你的 Google Gemini API 令牌。你可以通过 [Google AI Studio](https://makersuite.google.com/app/apikey) 获取一个 API 密钥。
```
(setq gemini-api-token "your api token")
```
4. 启动Gemini进程
```
(gemini-start-process)
```

大功告成🎉

## 用法

1. 安装 Python 依赖项：`pip install epc sexpdata google-generativeai`
2. 安装 [markdown-mode](https://github.com/jrblevin/markdown-mode)

*声明* : Gemini 目前支持的语言有限，故部分命令暂时无法实现

### 聊天
- 创建一个 markdown 文件（`*.mk` 或 `*.gemini`），并使用 `gemini-chat` 或 `gemini-chat-with-multiline` 与 Gemini 聊天

### 文档编辑
- `gemini-polish-document` 润色或填充文章内容
- `gemini-translate-into-chinese` 将文章翻译成中文。
- `gemini-translate-into-english` 将文章翻译成英文。

### 编码
- `gemini-generate-code` 根据选定的内容或输入生成代码
- `gemini-adjust-code` 根据输入调整代码内容
- `gemini-explain-code` 解释缓冲区中的代码
- `gemini-comment-code` 为缓冲区中的代码添加注释
- `gemini-refactory-code` 重构缓冲区中的代码
- `gemini-generate-commit-message` 插入提交消息

### 草稿
- 只需 `M-x gemini-choose-drafts` 并跳转到您喜欢的草稿即可！

## 反馈问题
请用命令 `emacs -Q` 并只添加 gemini.el 配置做一个对比测试，如果 `emacs -Q` 可以正常工作，请检查你个人的配置文件。

如果`emacs -Q`环境下问题依旧，请到[这里](https://github.com/AllTheLife/Gemini.el/issues/new) 反馈, 并附带 `*gemini*` 窗口的内容给我们提交 issue，那里面有很多线索可以帮助我们排查问题。

由于开发者是面临这中考压力的中学牲，所以不保证 bug 能够及时修，也不保证 pull requests 可以及时接收，望见谅。

如果有大佬愿意贡献一些代码，将感激不尽。

## 鸣谢
- 感谢 @manateelazycat 大佬的 [mind-wave](https://github.com/manateelazycat/mind-wave)，本仓库的大部分代码都来自于这里，它优美的代码对这个插件的开发提供了巨大帮助

## 贡献者

<a href = "https://github.com/AllTheLife/Gemini.el/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=AllTheLife/Gemini.el"/>
</a>
