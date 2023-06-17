English Version | [简体中文](./README.zh-CN.md)

# Bard.el
Enjoy using Google Bard in Emacs😀

## Dependencies
1. Reside in the United States or have a VPN that can connect to the US.
2. Have a Google account and access to Bard

## Installation
1. Open https://bard.google.com, press `F12` to open the console, select Applications -> Cookie, and copy the value of `__Secure-1PSID` (be careful not to copy it incorrectly)
2. Set the environment variable BARD_TOKEN to the value you just copied
(or save its value to `~/.emacs.d/bard/bard_cookie_token.txt`)
3. Install Python dependencies: `pip install epc sexpdata GoogleBard`
4. Install [markdown-mode](https://github.com/jrblevin/markdown-mode)
5. Use `git clone` to download this repository and replace the `load-path` path in the configuration below
6. Add the following code to your configuration file `~/.emacs`:
```elisp
(add-to-list 'load-path "<path-to-bard>")

(require 'bard)
```

## Usage

*Note*: Bard currently has limited language support, so some commands cannot be implemented temporarily.

### Chatting
- Create a `*.bard` file and chat with Bard using `bard-chat` or `bard-chat-with-multiline`
**tips**: Bard currently only supports English

### Document Editing
- `bard-polish-document` polishes or fills in the content of the article

### Coding
- `bard-generate-code` generates code based on selected content or input
- `bard-adjust-code` adjusts the code content based on input
- `bard-explain-code` explains the code in the buffer
- `bard-comment-code` adds comments to the code in the buffer
- `bard-refactory-code` refactors the code in the buffer
- `bard-generate-commit-message` insert the commit message

### Drafts
- Just `M-x bard-choose-drafts` and jump to the draft you like!

## Feedback
Please use the command `emacs -Q` and only add the bard.el configuration to do a comparative test. If `emacs -Q` can work normally, please check your personal configuration file.

If the problem still exists in the `emacs -Q` environment, please go to [here](https://github.com/AllTheLife/Bard.el/issues/new) to submit an issue with the content of the `*bard*` window attached, which contains many clues to help us troubleshoot the problem.

As the developer is a high school student facing exam pressure, it is not guaranteed that bugs can be fixed in a timely manner, nor that pull requests can be accepted in a timely manner. Thank you for your understanding.

If there are any big shots willing to contribute some code, it will be greatly appreciated.

## Acknowledgments
- Thanks to @manateelazycat for the beautiful code of [mind-wave](https://github.com/manateelazycat/mind-wave). Most of the code in this repository comes from here, which has greatly helped the development of this plugin.
- Thanks to @acheong08 for the excellent [API](https://github.com/acheong08/Bard) developed.

## Contributors

<a href = "https://github.com/AllTheLife//graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=AllTheLife/Bard.el"/>
</a>
