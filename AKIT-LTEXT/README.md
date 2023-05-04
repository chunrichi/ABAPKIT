# AKIT-LTEXT

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
