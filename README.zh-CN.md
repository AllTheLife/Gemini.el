[English Version](./README.md) | 简体中文

### Gemini:
# Gemini.el
在 Emacs 中享受使用 Google Gemini 😀

*注意*：这是一个修改过的[Bard.el](https://github.com/AllTheLife/Bard.el)版本， 用 Gemini 取代了 Bard。

## 依赖项
1. 拥有 Google 帐户并可以访问 Gemini

## 安装
1. 您需要准备好您的 Google Gemini API 令牌。
2. 安装 Python 依赖项：`pip install epc sexpdata google-generativeai`
3. 安装 [markdown-mode](https://github.com/jrblevin/markdown-mode)

## 用法

*注意*：Gemini 目前支持的语言有限，因此某些命令暂时无法实现。

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
