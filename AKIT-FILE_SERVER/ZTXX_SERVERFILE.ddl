@EndUserText.label : '服务器中的文件'
@AbapCatalog.enhancement.category : #NOT_CLASSIFIED
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztxx_serverfile {

  key mandt        : mandt not null;
  key guid         : guid not null;
  aliass           : text50;
  file_name        : text50;
  file_extension   : text50;
  file_length      : int4;
  file_path        : text150;
  file_rootpath    : text150;
  file_subpath     : text150;
  file_fullname    : text50;
  upload_file_name : text50;
  mime_type        : text10;
  delete_flag      : boolean;
  create_date      : datum;
  create_time      : uzeit;
  create_user      : uname;
  change_date      : datum;
  change_time      : uzeit;
  change_user      : unam;

}