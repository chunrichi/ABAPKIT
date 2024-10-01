# AKIT-DING_HOOK_BOT

通过调用钉钉 **群机器人** 接口，实现消息推送到群中

**注意** 用于钉钉使用的是 `HTTPS` 协议，需要在系统事务码 `STRUST` 中添加飞书相关 SSL 证书

## 实现方式

通过调用钉钉机器人的接口，将固定内容发送到群组中

## 用途

在使用钉钉的客户中，更方便的消息推送

自定义机器人的优点

- 更直观的群消息通知和相关人员的添加
- 无需企业用户的开发权限，只需要将机器人添加到群组中，并更新 `URL` 和 `Token` 到程序中即可

## 使用方法

将 `ZCL_AKIT_DING_HOOK_BOT` 导入系统即可

## 示例

各种类型的消息推送 [ZAKIT_DEMO_DING_HOOKBOT](./ZAKIT_DEMO_DING_HOOKBOT.abap)

需要先按 [添加机器人](#添加机器人) 将机器人添加到群组中，然后将机器人相关的 `URL` 和 `Token` 添加到执行弹框中

---

## 操作流程

### 添加机器人

![设置](attachments/im20230531110731.png)

![机器人](attachments/im20230531110804.png)

![添加机器人](attachments/im20230531110828.png)

![添加机器人](attachments/im20230531110901.png)

![自定义机器人](attachments/im20230531110922.png)

![机器人详情](attachments/im20230531110950.png)

按需选择加签 [加签设置](https://open.dingtalk.com/document/robots/customize-robot-security-settings)

![设置机器人](attachments/im20230531111040.png)

得到 hook 地址 [设置说明](https://open.dingtalk.com/document/robots/custom-robot-access)

![机器人配置信息](attachments/im20230531111131.png)

### 程序处理

> 由于编写系统时戳不准确（需要精确到毫秒），此处采用关键词进行测试

![设置关键词](attachments/im20230601100835.png)

执行程序

![SE38](attachments/im20230601100849.png)

填写 `URL` 、选择类型、点击执行

![填写信息](attachments/im20230601100916.png)

效果展示

![效果](attachments/im20230601101016.png)
