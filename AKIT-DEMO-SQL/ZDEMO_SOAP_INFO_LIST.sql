SELECT
    cfg.client,
    cfg.type,
    cfg.state,
    cfg.config_name,
    cfg.dt_obj_name,
    cfg.config_name_ext,
    cfg.config_key,
    fun.function,
    hash.prop_value AS hash_key,
    auth.prop_value AS auth,
    CASE cfg.type WHEN 'CR' THEN CONCAT(CONCAT(CONCAT(CONCAT(CONCAT(cr.Protocol,'://'),cr.Host),':'),CASE cr.Port WHEN '' THEN '80' ELSE cr.Port END ),cr.Path) ELSE srv.url END AS url
    FROM srt_cfg_dir AS cfg
    LEFT JOIN vepfunction AS fun ON fun.vepname = cfg.dt_obj_name AND fun.version = cfg.state
    LEFT JOIN srt_rtc_data AS hash ON hash.client = cfg.client AND hash.prop_value = cfg.config_key AND hash.prop_name = 'ConfigKey'
    LEFT JOIN srt_rtc_data AS auth ON auth.client = cfg.client AND auth.binding_key = hash.binding_key AND auth.prop_name = 'AuthenticationMethod'
    LEFT JOIN srt_cfg_srv_asgn AS srv ON srv.client = cfg.client AND srv.binding_key = hash.binding_key
    LEFT JOIN ( SELECT client,
                       binding_key,
                       max( CASE prop_name WHEN 'URLHost'     THEN prop_value ELSE '' END ) AS Host,
                       max( CASE prop_name WHEN 'URLLanguage' THEN prop_value ELSE '' END ) AS Language,
                       max( CASE prop_name WHEN 'URLPath'     THEN prop_value ELSE '' END ) AS Path,
                       max( CASE prop_name WHEN 'URLPort'     THEN prop_value ELSE '' END ) AS Port,
                       max( CASE prop_name WHEN 'URLProtocol' THEN prop_value ELSE '' END ) AS Protocol
                FROM srt_rtc_data WHERE prop_name LIKE 'URL%'
                GROUP BY client, binding_key ) AS cr ON cr.client = hash.client
                                                    AND cr.binding_key = hash.binding_key