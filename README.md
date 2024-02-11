English Version | [简体中文](./README.zh-CN.md)

# Gemini.el
Enjoy using Google Gemini in Emacs😀

## Dependencies
1. Have a Google account and access to Gemini

## Installation
1. You need to have your Google Gemini API token ready. You can get an API key via [Google AI Studio](https://makersuite.google.com/app/apikey).
```
(setq gemini-api-token "your api token")
```
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

## Feedback
Please use the command `emacs -Q` and only add the gemini.el configuration to do a comparative test. If `emacs -Q` can work normally, please check your personal configuration file.

If the problem still exists in the `emacs -Q` environment, please go to [here](https://github.com/AllTheLife/Gemini.el/issues/new) to submit an issue with the content of the `*gemini*` window attached, which contains many clues to help us troubleshoot the problem.

As the developer is a high school student facing exam pressure, it is not guaranteed that bugs can be fixed in a timely manner, nor that pull requests can be accepted in a timely manner. Thank you for your understanding.

If there are any big shots willing to contribute some code, it will be greatly appreciated.

## Acknowledgments
- Thanks to @manateelazycat for the beautiful code of [mind-wave](https://github.com/manateelazycat/mind-wave). Most of the code in this repository comes from here, which has greatly helped the development of this plugin.
- Thanks to @acheong08 for the excellent [API](https://github.com/acheong08/Bard) developed.

## Contributors

<a href = "https://github.com/AllTheLife/Gemini.el/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=AllTheLife/Gemini.el"/>
</a>
