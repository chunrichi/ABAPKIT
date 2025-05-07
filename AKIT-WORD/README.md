# AKIT-WORD

> 通过 OpenXML 来实现的 Word 内容替换

**注意** 目前只支持 Word 的文本替换

## 实现方式

- 通过 Word 的开发模式编辑 Word 文档的信息
- 通过 cl_abap_zip 获取 Word 中的文件信息
- 通过 ixml 进行 sdt 的替换检索

## 使用方式

### Word 文件模板创建

- 创建 Word 文档模板
- 打开 开发工具
- 打开开发工具中：设计模式
- 使用 格式文本控件 和 重复分区内容控件 + 属性 编辑标签，完成模板编辑
- 将文档通过 SMW0 上载到 ABAP 系统中

### Word 文档模板信息提取转 ABAP 数据结构

- 通过 ZAKIT_DOCX_READ_TEMP 程序读取模板信息，并生成 ABAP 代码

### Word 文档内容替换

- 参考 ZAKIT_DOCX_WRITE_TEMP 程序完成程序的编写
- 执行下载查看
