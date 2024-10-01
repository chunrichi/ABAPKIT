# AKIT-FEISHU-HOOK-BOT

通过调用飞书 **自定义机器人** 接口，实现消息推送到群中

**注意** 用于飞书使用的是 `HTTPS` 协议，需要在系统事务码 `STRUST` 中添加飞书相关 SSL 证书

## 实现方式

通过调用飞书的接口，将固定内容发送到群组中

## 用途

在使用飞书的客户中，更方便的消息推送

自定义机器人的优点

- 更直观的群消息通知和相关人员的添加
- 无需企业用户的开发权限，只需要将机器人添加到群组中，并更新 `URL` 和 `Token` 到程序中即可

## 使用方法

将 `ZCL_AKIT_FEISHU_HOOK_BOT` 导入系统即可

## 示例

各种类型的消息推送 [ZAKIT_DEMO_FEISHU_HOOKBOT](./ZAKIT_DEMO_FEISHU_HOOKBOT.abap)

需要先按 [自定义机器人操作流程](https://open.feishu.cn/document/ukTMukTMukTM/ucTM5YjL3ETO24yNxkjN#7a28964d) 将机器人添加到群组中，然后将机器人相关的 `URL` 和 `Token` 添加到执行弹框中

---

## 操作流程

选择设置 -> 群机器人

![添加机器人](attachments/im20230517154928.png)

选择添加机器人 -> 拷贝拿到的 webhook 和 签名 (签名是可选的，可不勾选)

![获取地址](attachments/im20230517155043.png)

执行程序 `ZAKIT_DEMO_FEISHU_HOOKBOT`

![执行程序](attachments/im20230517155255.png)

选择要推送的类型，点执行，填写刚才拿到的 `URL` 和 `TOKEN` ( `TOEKN` 可不填 )

![输入hook](attachments/im20230517155315.png)

左下角会出现相应的消息，如果成功，可在飞书中查看推送的内容

![查看推送内容](attachments/im20230517155500.png)
