# AKIT-JOB

将 `JOB_OPEN` 和 `JOB_CLOSE` 进行简单的封装，并增加了一些常见的功能（查询、删除）

**注意** 使用时应避免名称重复

## 实现方式

通过 `JOB_OPEN` 和 `JOB_CLOSE` 函数进行 JOB 的创建

## 用途

- 创建基于 Report 的JOB (SUBMIT)
- 创建标准程序的 JOB
- 指定执行周期
- 指定开始时间
- 指定多个关联的 JOB

## 使用方法

1. 将 `ZCL_AKIT_JOB` 导入到系统
2. 将 `ZCL_AKIT_JOB-CCIMP` 导入到类的局部方法中

## 示例

[ZAKIT_DEMO_JOB](./ZAKIT_DEMO_JOB.abap)

可直接多次执行 `ZAKIT_DEMO_JOB` 程序查看效果

1. 第一次执行创建 JOB
2. 第二次执行检查 JOB 是否存在
   1. 显示当前执行周期
   2. 确认是否删除 JOB
