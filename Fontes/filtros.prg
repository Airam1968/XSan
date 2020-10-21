*** filtros da consulta de recebimentos
FUNCTION FILTROS()
      DO CASE
         CASE ePERIODO = "T"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         CASE ePERIODO = "A"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI)
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         CASE ePERIODO = "P"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         CASE ePERIODO = "M"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "P"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        CASE ePROTESTO = "N"
                           SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "P"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                           CASE ePROTESTO = "N"
                              SET FILTER TO  (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. ATIVA .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM)
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
      ENDCASE
      GOTO TOP
return
