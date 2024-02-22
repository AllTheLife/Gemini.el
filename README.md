English Version | [简体中文](./README.zh-CN.md)

# Gemini.el
Enjoy using Google Gemini in Emacs😀

## Dependencies
1. Have a Google account and access to Gemini

## Installation

This package will not be uploaded to melpa, thanks for your understanding.

1.  Firstly, clone this repository `git clone https://github.com/AllTheLife/Gemini.el <path-to-site-lisp>`
2.  Load the package in emacs
```lisp
(add-to-list 'load-path "<path-to-gemini>")
(require 'gemini)
```
3. You need to have your Google Gemini API token ready. You can get an API key via [Google AI Studio](https://makersuite.google.com/app/apikey).
```
(setq gemini-api-token "your api token")
```
4. (optional) You may need to set up a proxy if you are not in a region or country Google Gemini allowed or in order to accelerate Google API request.
```
(setq gemini-http-proxy "http://localhost:port")
```
5. Start gemini process
```
(gemini-start-process)
```

Et voilà 🎉

## Usage

1. Install Python dependencies: `pip install epc sexpdata google-generativeai`
2. Install [markdown-mode](https://github.com/jrblevin/markdown-mode)

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

## Contributors

<a href = "https://github.com/AllTheLife/Gemini.el/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=AllTheLife/Gemini.el"/>
</a>
