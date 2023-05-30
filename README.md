# ABAP KIT

常用的 ABAP 功能的封装和说明文档

> Packaging and documentation of common ABAP functions

## 目录

- [ABAP KIT](#abap-kit)
  - [目录](#目录)
  - [工具说明](#工具说明)
    - [AKIT-ITAB2XLSX](#akit-itab2xlsx)
    - [AKIT-EMAIL](#akit-email)
    - [AKIT-JOB](#akit-job)
    - [AKIT-LTEXT](#akit-ltext)
    - [AKIT-FEISHU-HOOK-BOT](#akit-feishu-hook-bot)
    - [AKIT-WEIXIN-HOOK-BOT](#akit-weixin-hook-bot)
    - [AKIT-FILE-SERVER](#akit-file-server)
    - [AKIT-HTTP](#akit-http)

## 工具说明

每个工具的大概说明及链接

### [AKIT-ITAB2XLSX](./AKIT-ITAB2XLSX)

将 ABAP 内表转换成 excel，不依赖于本地的 excel，可直接拿到二进制数据。（通过标准的 alv 功能将内表转换为 XLSX 文件。需要传参 itab 和 alv 的 fieldcat 从而转换内容。）

用途

1. 邮件附件
2. 大数据量的文件导出

---

### [AKIT-EMAIL](./AKIT-EMAIL)

邮件发送封装，通过 `cl_cam_address_bcs` 处理的简单封装的邮件发送

简化了发送邮件的流程（原本流程也很简单）

---

### [AKIT-JOB](./AKIT-JOB)

将 SM36 相关创建功能的简单一个封装，可通过类 `ZCL_AKIT_JOB` 快速的创建、删除、查询 JOB

---

### [AKIT-LTEXT](./AKIT-LTEXT)

通过对 BLOB 表的二进制数据的读取，快速的拉取大量的长文本内容，无需每次 `READ_TEXT` 访问数据库，从而加快读取速度

---

### [AKIT-FEISHU-HOOK-BOT](./AKIT-FEISHU_HOOK_BOT)

通过调用飞书 **自定义机器人** 接口，实现消息推送到群中

官方文档: [飞书自定义机器人](https://open.feishu.cn/document/ukTMukTMukTM/ucTM5YjL3ETO24yNxkjN#4996824a)

---

### [AKIT-WEIXIN-HOOK-BOT](./AKIT-WEIXIN_HOOK_BOT)

通过调用企业微信 **群机器人** 接口，实现消息等内容推送到群中

官方文档: [群机器人配置说明](https://developer.work.weixin.qq.com/document/path/91770)

---

### [AKIT-FILE-SERVER](./AKIT-FILE_SERVER)

通过 `OPEN DATASET` 相关方式往服务器指定目录写入文件

可通过服务器目录映射将文件进而写入特定服务器中，从而不占用 SAP 应用服务器内存

---

### [AKIT-HTTP](./AKIT-HTTP)

简单的 `HTTP` 示例，简单的模拟了 `REST` 请求的一些比较常见的内容

> 如果使用可将局部类拷出
