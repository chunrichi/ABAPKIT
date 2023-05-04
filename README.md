# ABAP KIT

常用的 ABAP 功能的封装和说明文档

> Packaging and documentation of common ABAP functions

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
