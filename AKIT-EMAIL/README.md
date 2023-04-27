# AKIT EMAIL

邮件发送封装

## 实现方式

通过 `cl_bcs` 和 `cl_document_bcs` 进行邮件发送，可直接触发立即发送

## 使用方式

调用 `ZCL_AKIT_EMAIL` 的 `SENDMAIL` 方法直接进行发送

## 示例

[ZAKIT_DEMO_EMAIL](./ZAKIT_DEMO_EMAIL.abap)

主要流程为

1. 设置发送者信息
2. 设置发送title
3. 设置抄送等信息
4. 设置正文（可选择 HTML 和 文本 类型）
5. 调用发送方法

## 注意

1. 需要系统中有账号维护了发送邮箱
2. 需要维护发送邮件的相关配置
