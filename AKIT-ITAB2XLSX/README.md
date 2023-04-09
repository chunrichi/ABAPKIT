# AKIT-ITAB2XLSX

将 ABAP 内表转换成 excel，不依赖于本地的 excel，可直接拿到二进制数据。

**注意** 该方法无法设置样式

## 实现方式

通过 `cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform` 将内表和 `fieldcat` 设置的表头作为导出 excel

## 用途

1. 邮件附件
2. 大数据量的文件导出

## 示例

[ZAKIT_DEMO_ITAB2XLSX](./ZAKIT_DEMO_ITAB2XLSX.abap)

主要流程为

1. 设置 `fieldcat` 用于列名的设置
2. 转换内标为 xstring
3. 可选，将 xstring 导出到本地
