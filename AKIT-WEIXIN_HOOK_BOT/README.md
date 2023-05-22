# AKIT-WEIXIN-HOOK-BOT

通过调用企业微信 **群机器人** 接口，实现消息推送到群中

**注意** 用于企业微信使用的是 `HTTPS` 协议，需要在系统事务码 `STRUST` 中添加企业微信相关 SSL 证书

## 实现方式

通过调用企业微信的接口，将固定内容发送到群组中

## 用途

在使用企业微信的客户中，更方便的消息推送

群机器人的优点

- 更直观的群消息通知和相关人员的添加
- 无需企业用户的开发权限，只需要将机器人添加到群组中，并更新 `URL` 到程序中即可

## 使用方法

将 `ZCL_AKIT_WEIXIN_HOOK_BOT` 导入系统即可

## 示例

各种类型的消息推送 [ZAKIT_DEMO_WEIXIN_HOOKBOT](./ZAKIT_DEMO_WEIXIN_HOOKBOT.abap)

需要先按将机器人添加到群组中，然后将机器人相关的 `URL` 添加到执行弹框中

---

## 操作流程

选择设置 -> 群机器人

![添加机器人-设置](attachments/im20230522105127.png)

选择创建一个新的机器人

![添加机器人-创建](attachments/im20230522105156.png)

设置机器人名称

![添加机器人-命名](attachments/im20230522105226.png)

拿到 hook 地址

![添加机器人-hook地址](attachments/im20230522105252.png)

如果遗忘可以在这里查看

![添加机器人-创建后查看地址](attachments/im20230522105330.png)

执行程序 `ZAKIT_DEMO_WEIXIN_HOOKBOT`

![执行测试程序](attachments/im20230522105346.png)

选择要推送的类型，点执行，填写刚才拿到的 `URL`

![设置hook地址](attachments/im20230522105410.png)

左下角会出现相应的消息，如果成功，可在企业微信中查看推送的内容

![查看推送消息](attachments/im20230522105515.png)
