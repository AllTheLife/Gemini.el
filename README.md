English Version | [简体中文](./README.zh-CN.md)

# Gemini.el
Enjoy using Google Gemini in Emacs😀

*Note*: This is a modified version of [Bard.el](https://github.com/AllTheLife/Bard.el) that replaces Bard with Gemini.

## Dependencies
1. Have a Google account and access to Gemini

## Installation
1. You need to have your Google Gemini API token ready.
2. Install Python dependencies: `pip install epc sexpdata google-generativeai`
3. Install [markdown-mode](https://github.com/jrblevin/markdown-mode)
```

## Usage

*Note*: Gemini currently has limited language support, so some commands cannot be implemented temporarily.

### Chatting
- Create a markdown file (`*.mk` or `*.gemini`) and chat with Gemini using `gemini-chat` or `gemini-chat-with-multiline`

### Document Editing
- `gemini-polish-document` polishes or fills in the content of the article
- `gemini-translate-into-chinese` Translate the passage into Chinese.
- `gemini-translate-into-english` Translate the passage into English.

### Coding
- `gemini-generate-code` generates code based on selected content or input
- `gemini-adjust-code` adjusts the code content based on input
- `gemini-explain-code` explains the code in the buffer
- `gemini-comment-code` adds comments to the code in the buffer
- `gemini-refactory-code` refactors the code in the buffer
- `gemini-generate-commit-message` insert the commit message

### Drafts
- Just `M-x gemini-choose-drafts` and jump to the draft you like!

