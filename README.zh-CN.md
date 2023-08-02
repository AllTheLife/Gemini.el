[English Version](./README.md) | 简体中文

# Bard.el
享受在 Emacs 中使用 Google Bard 的快乐吧😀

## 依赖
1. 身居美利坚或拥有一个稳定的漂亮国🪜
2. 拥有 Google 账号并拥有访问 Bard 的权限

## 安装
1. 打开 https://bard.google.com ,
   - 按下 `F12` 打开 console
   - 选择 Applications -> Cookies
     - 复制`__Secure-1PSID` 和 `__Secure-1PSIDTS` 的值（注意不要复制错了）
2. 设置环境变量 `BARD_TOKEN` 和 `BARD_TOKEN_TS` 为刚才复制的值
（或者将其值保存到 `~/.emacs.d/bard/bard_cookie_token.txt`， 格式为：`__Secure-1PSID`,`__Secure-1PSIDTS`）
3. 安装 Python 依赖：`pip install epc sexpdata GoogleBard`
4. 安装 [markdown-mode](https://github.com/jrblevin/markdown-mode)
5. 使用 `git clone` 下载此仓库，并替换下面配置中的 `load-path` 路径
6. 将以下代码添加到您的配置文件 `~/.emacs` 中：
```elisp
(add-to-list 'load-path "<path-to-bard>")
;; (setq bard-http-proxy "http://localhost:port") ;; You may need to set up a proxy if you are not in a region or country Google Bard allowed.
(require 'bard)
```

### 自动提取 cookie

除了从环境变量或配置文件中读取 `BARD_TOKEN` 和 `BARD_TOKEN_TS` 之外，`Bard.el` 现在还可以自动从您的浏览器中提取 cookie。这对于不想手动设置环境变量的人来说很有用。

要使用此功能，您需要安装 `browser_cookie3` Python 包。安装了该包后，您可以运行以下命令来提取 cookie：

`pip install browser-cookie3`

**注意：** 如果您使用的是 `browser_cookie3` 不支持的浏览器，则还是需要手动设置 `BARD_TOKEN` 和 `BARD_TOKEN_TS`。

## 使用

*声明* : Bard 目前支持的语言有限，故部分命令暂时无法实现

### 聊天
- 新建 `*.bard` 文件，通过 `bard-chat` 或 `bard-chat-with-multiline` 来与 Bard 聊天

### 修改文档
- `bard-polish-document` 润色或填充文章内容
- `bard-translate-into-chinese` 把选段翻译成中文
- `bard-translate-into-english` 把选段翻译成英文

### Coding
- `bard-generate-code` 根据选中内容或输入生成代码
- `bard-adjust-code` 根据输入调整代码内容
- `bard-explain-code` 解释 buffer 中的代码
- `bard-comment-code` 为 buffer 中的代码添加注释
- `bard-refactory-code` 重构 buffer 中的代码
- `bard-generate-commit-message` 插入建议的提交信息

### 选择回答
- 只需要 `M-x bard-choose-drafts`!

## 反馈问题
请用命令 `emacs -Q` 并只添加 bard.el 配置做一个对比测试，如果 `emacs -Q` 可以正常工作，请检查你个人的配置文件。

如果`emacs -Q`环境下问题依旧，请到[这里](https://github.com/AllTheLife/Bard.el/issues/new) 反馈, 并附带 `*bard*` 窗口的内容给我们提交 issue，那里面有很多线索可以帮助我们排查问题。

由于开发者是面临这中考压力的中学牲，所以不保证 bug 能够及时修，也不保证 pull requests 可以及时接收，望见谅。

如果有大佬愿意贡献一些代码，将感激不尽。

## 鸣谢
- 感谢 @manateelazycat 大佬的 [mind-wave](https://github.com/manateelazycat/mind-wave)，本仓库的大部分代码都来自于这里，它优美的代码对这个插件的开发提供了巨大帮助
- 感谢 @acheong08 大佬开发的优秀的 [API](https://github.com/acheong08/Bard)

## 贡献者

<a href = "https://github.com/AllTheLife/Bard.el/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=AllTheLife/Bard.el"/>
</a>
