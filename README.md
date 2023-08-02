English Version | [简体中文](./README.zh-CN.md)

# Bard.el
Enjoy using Google Bard in Emacs😀

## Dependencies
1. Reside in the United States or have a VPN that can connect to the US.
2. Have a Google account and access to Bard

## Installation
1. Go to https://bard.google.com/
    - F12 for console
    - Copy the values
      - Session: Go to Application → Cookies → `__Secure-1PSID` and `__Secure-1PSIDTS`. Copy the value of those cookie.
2. Set the environment variable BARD_TOKEN (`__Secure-1PSID`) and BARD_TOKEN_TS (`__Secure-1PSIDTS`) to the value you just copied
(or save its value to `~/.emacs.d/bard/bard_cookie_token.txt` with format: `__Secure-1PSIDTS`,`__Secure-1PSIDTS`)
3. Install Python dependencies: `pip install epc sexpdata GoogleBard`
4. Install [markdown-mode](https://github.com/jrblevin/markdown-mode)
5. Use `git clone` to download this repository and replace the `load-path` path in the configuration below
6. Add the following code to your configuration file `~/.emacs`:
```elisp
(add-to-list 'load-path "<path-to-bard>")
;; (setq bard-http-proxy "http://localhost:port") ;; You may need to set up a proxy if you are not in a region or country Google Bard allowed.
(require 'bard)
```

### Automatic Cookie Extraction

In addition to reading the `BARD_TOKEN` and `BARD_TOKEN_TS` environment variables or configuration file, `Bard.el` can now automatically extract the cookies from your browser. This is useful if you don't want to manually set the environment variables.

To use this feature, you need to install the `browser_cookie3` Python package. Once you have installed the package, you can run the following command to extract the cookies:

`pip install browser-cookie3`

**Note:** If you are using a browser that is not supported by [browser_cookie3](https://github.com/borisbabic/browser_cookie3), you will need to manually set the `BARD_TOKEN` and `BARD_TOKEN_TS`.

## Usage

*Note*: Bard currently has limited language support, so some commands cannot be implemented temporarily.

### Chatting
- Create a `*.bard` file and chat with Bard using `bard-chat` or `bard-chat-with-multiline`

### Document Editing
- `bard-polish-document` polishes or fills in the content of the article
- `bard-translate-into-chinese` Translate the passage into Chinese.
- `bard-translate-into-english` Translate the passage into English.

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

<a href = "https://github.com/AllTheLife/Bard.el/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=AllTheLife/Bard.el"/>
</a>
