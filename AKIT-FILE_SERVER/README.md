# AKIT-FILE_SERVER

通过 `OPEN DATASET` 将数据写入到 SAP 应用服务器中。

**注意** 需要配置两个地方 `AL11` 和 `SM49`

## 实现方式

通过 `OPEN DATASET` 等相关方法，将二进制数据写入 SAP 应用服务器中，也可以通过 linux 目录映射，将文件实际写入其他服务器中

常见的 SAP 将该功能作为日志写入的地方

## 用途

没有使用 SAP Content Server 时关于小批量文件的上传共享存储

> 还可以用 ftp 的方式

## 使用方法

将 `ZCL_AKIT_FILE_SERVER` 导入到系统即可，使用方法从参考 `ZAKIT_DEMO_FILE_SERVER`

方法说明

- GEN_GUID 公有 生成 `UUID` 和 `GUID`
- DELETE   公有 通过文件名删除上载的文件
- DIRHOME  公有 获取根目录（AL11配置的）
- DOWNLOAD 公有 下载文件(拿到二进制数据)
- GEN_INFO 公有 获取文件信息
- MKDIR    公有 创建子目录
- UPLOAD   公有 上载文件
- GUIDOWN  私有 GUI 下载

## 示例

[ZAKIT_DEMO_FILE_SERVER](./ZAKIT_DEMO_FILE_SERVER.abap)

## 配置

### AL11 配置

点击红框内的按钮 配置用户目录

![配置用户目录](attachments/im20230529133341.png)

维护用户目录

![维护用户目录](attachments/im20230529133404.png)

直接保存即可

![保存](attachments/im20230529133246.png)

### SM49

![SM49](attachments/im20230529133438.png)

此处配置 ZMKDIR 用于创建文件夹

创建外部命令

![创建外部命令](attachments/im20230529133539.png)

创建文件夹命令ZMKDIR的维护

![ZMKDIR的维护](attachments/im20230529133559.png)
