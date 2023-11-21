# ABAP SCANER

ABAP 扫描器

## 目的

通过 python + 维护目录，找到所有关心的内容

## 使用

1. 创建 `config.json` 文件，并添加配置（可参考`demoConfig.json`）
   1. `folderPaths`     []string   : 检索路径
   2. `regexPattern`    string     : 正则表达式
   3. `outputFile`      string     : 输出文件名
   4. `includeDirs`     []string   : 检索子文件夹名(空为所有)
2. 运行程序即可
