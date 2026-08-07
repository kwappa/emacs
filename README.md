# emacs

ターミナル専用の Emacs 設定 (Emacs 30+ 前提)。

2013 年まで使っていた旧設定は `main` ブランチの履歴に残っている
(`git log --all -- init.el` などで参照可能)。

## セットアップ

```sh
brew install emacs
git clone git@github.com:kwappa/emacs.git ~/src/github.com/kwappa/emacs
ln -s ~/src/github.com/kwappa/emacs ~/.config/emacs
```

初回起動時に MELPA から必要なパッケージが自動でインストールされる。

## 構成

```
early-init.el         起動前設定 (GC 抑制など)
init.el               エントリポイント (package.el 設定とモジュール読み込み)
lisp/
  my-defaults.el      編集の基本 (文字コード・インデント・スクロール・dired)
  my-keys.el          グローバルキーバインド
  my-completion.el    ミニバッファ補完 (vertico + orderless + marginalia + consult)
  my-ui.el            テーマ・不可視文字の可視化
  my-lang.el          言語モード (使うものから都度追加)
```

## 主なキーバインド

| キー | 動作 |
|------|------|
| `C-h` | Backspace (ヘルプは `F1`) |
| `M-o` | ウインドウ間移動 |
| `M-n` / `M-p` | 1行スクロール |
| `M-y` | kill-ring から選んで貼り付け (consult) |
| `M-s l` / `M-s r` | バッファ内検索 / ripgrep |
| `C-x C-r` | 最近使ったファイル |
| `C-c n` | 行番号表示のトグル |
| `F8` | 折り返しのトグル |
| `F3` / `F4` / `F5` | キーボードマクロ 記録 / 終了・再生 / 再生 |
