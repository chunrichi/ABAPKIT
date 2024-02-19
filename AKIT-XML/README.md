# AKIT-XML

模仿 `/ui2/cl_json` 实现简单的 ABAP 到 XML 的转换

**注意** 目前只支持 ABAP2XML，XML2ABAP由于结构问题尚未处理，添加后会更改程序命名

## 实现方式

通过解析 ABAP 结构，实现生成 XML 树，并转换为 STRING
