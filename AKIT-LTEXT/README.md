# AKIT-LTEXT

> 由于 751 版本中不支持直接从二进制表拉取数据改为采用标准的批量读取长文件的方法
> 可将 zcl_akit_sltext 替换掉 zcl_akit_ltext 方法（效率比较如果有空可以比较看看）

直接从表 `stxl` 中读取二进制数据，解析放在程序中执行。

由于只访问一次数据库，从而加快了数据读取的速度

**注意** 因为一次拉取的数据量大，可能较为占用运行空间（相对而言，其实还好）

## 实现方式

通过 `IMPORT FROM DATA BUFFER` 的方式处理读取二进制数据

## 用途

再读取大量的长文本时，避免使用 `READ_TEXT` 函数从而加快读取速度

场景: 各种订单的长文本读取

## 使用方法

将 `ZCL_AKIT_LTEXT` 导入系统即可

## 示例

执行速度 [ZAKIT_DEMO_LTEXT01](./ZAKIT_DEMO_LTEXT01.abap)
空间占用 [ZAKIT_DEMO_LTEXT02](./ZAKIT_DEMO_LTEXT02.abap)
