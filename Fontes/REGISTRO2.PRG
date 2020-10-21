*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REGISTRO.PRG
* Data        : 01/01/99
* Objetivo    : GERACAO DO REGISTRO TRANSA‡Æo PARA ARQUIVO REMESSA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REGISTRO2()
DO CASE
   CASE eNUM_BANCO = "001"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,5)+SUBSTR(eCOD_CC,1,9)+"047286"+eESP_IDENT+eINDENT+REPLICATE("0",16)+SPACE(4)+"SD "+;
        "0191000000000000"+SPACE(5)+"3101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"0010000001A"+eDT_EMIS+REPLICATE("0",62)+;
        eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,37)+SPACE(3)+SUBSTR(eENDERECO,1,37)+SPACE(15)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,40)+SPACE(3)+eREG_SEQ
   CASE eNUM_BANCO = "008"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,5)+"000000"+SUBSTR(eCOD_CC,1,9)+"00"+eESP_IDENT+eINDENT+SPACE(45)+;
        "301"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"008"+REPLICATE("0",5)+"01"+"N"+;
        eDT_EMIS+IF(PROTESTO,"0610","0001")+"0000000000033"+REPLICATE("0",45)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,40)+SPACE(2)+"1"+eREG_SEQ
   CASE eNUM_BANCO = "033"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,8)+SPACE(9)+eESP_IDENT+eINDENT+SUBSTR(eCOD_AGENCI,2,3)+REPLICATE("0",7)+;
        SPACE(35)+"101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"033"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"06","07")+"58"+"0000000000033"+REPLICATE("0",45)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;                    
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;                               
        eMSG+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+SPACE(1)+eREG_SEQ
   CASE eNUM_BANCO = "104"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))            
        eCPF_CGC     = REPLICATE("0",14-eTAM_CPF_CGC)+eCPF_CGC
        eINDENT = REPLICATE(" ",25-LEN(ALLTRIM(eINDENT)))+ALLTRIM(eINDENT)
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(2)+"00"+eINDENT+REPLICATE("0",11)+SPACE(3)+;
        SUBSTR(eMSG,1,30)+"11"+"01"+ALLTRIM(eINDENT)+SPACE(2)+eDT_VENC+eVLR_PARCE+"104"+"00000"+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"01","02")+"00"+REPLICATE("0",13+6+13+13+13)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,35)+;
        SPACE(5)+SUBSTR(eENDERECO,1,35)+SPACE(5)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        eDT_MULT+REPLICATE("0",10)+REPLICATE(" ",22)+"00"+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+"1"+eREG_SEQ
   CASE eNUM_BANCO = "151"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))            
        eCPF_CGC     = REPLICATE("0",14-eTAM_CPF_CGC)+eCPF_CGC
        eINDENT = REPLICATE(" ",25-LEN(ALLTRIM(eINDENT)))+ALLTRIM(eINDENT)
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "15100013"+SUBSTR(eREG_SEQ,2,5)+"P 0100"+eCOD_AGENCI+"0000"+eCOD_CC+SPACE(5)+"098870"+"0000000000"+"052120"+;
        SPACE(3)+SUBSTR(eINDENT,13,13)+eDT_VENC+"00"+eVLR_PARCE+"00000002A"+eDT_EMIS+"3"+SPACE(8)+"00000000000100001"+REPLICATE("0",23)+SPACE(30)+;
        +ALLTRIM(eINDENT)+" "+IIF(PROTESTO,ePRAZO_PRO,"99")+SPACE(4)+"00"+SPACE(3)+"00"+SPACE(4)

        * Numero Sequencial do Registro
        eNUM_SEQ = eNUM_SEQ + 1
        eTAM_SEQ = LENNUM(eNUM_SEQ)
        eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)

        ? "15100013"+eREG_SEQ+"Q 0110"+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,15)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+"20"+sisCGC_CPF+SUBSTR(eCLIENTE,1,40)+SPACE(31)
   CASE eNUM_BANCO = "215"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+eCOD_AGENCI+"02"+SUBSTR(eCOD_CC,1,8)+SPACE(6)+eESP_IDENT+eINDENT+REPLICATE("0",16)+;
        "00"+SPACE(27)+"101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"215"+REPLICATE("0",5)+"15"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"16","20")+"00"+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        IIF(LEN(ALLTRIM(eENDERECO)) > 30,SUBSTR(eENDERECO,1,40),SUBSTR(eENDERECO,1,30)+SUBSTR(eBAIRRO,1,10))+IIF(PROTESTO,ePRAZO_PRO,"00")+ePRAZO_DEV+SPACE(8)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        eMSG+"99"+" "+eREG_SEQ
   CASE eNUM_BANCO = "230"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,10)+SPACE(7)+eESP_IDENT+eINDENT+REPLICATE("0",13)+;
        SPACE(7)+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+SPACE(23)+"101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"230"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"04","01")+"00"+REPLICATE("0",13)+REPLICATE("0",45)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        eMSG+REPLICATE(" ",2)+"9"+eREG_SEQ
   CASE eNUM_BANCO = "237"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,10)+SPACE(7)+eESP_IDENT+eINDENT+REPLICATE("0",13)+;
        SPACE(7)+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+SPACE(23)+"101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"230"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"04","01")+"00"+REPLICATE("0",13)+REPLICATE("0",45)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        eMSG+REPLICATE(" ",2)+"9"+eREG_SEQ
   CASE eNUM_BANCO = "275"
        *
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+"0"+SUBSTR(eCOD_AGENCI,1,4)+"0"+SUBSTR(eCOD_CC,1,7)+SPACE(7)+eINDENT+SPACE(17)+"0"+eINDENT+SPACE(24)+REPLICATE("0",9)+;
        SPACE(3)+"501"+"  "+eINDENT+eDT_VENC+eVLR_PARCE+"275"+REPLICATE("0",5)+"99"+"A"+eDT_EMIS+"0000"+REPLICATE("0",58)+;
        eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,40)+SPACE(1)+"77"+eREG_SEQ
        *
   CASE eNUM_BANCO = "291"
        * CALCULO DO DAC
*        eID_1 = VAL(SUBSTR(eINDENT,1,1))
*        eID_2 = VAL(SUBSTR(eINDENT,2,1))
*        eID_3 = VAL(SUBSTR(eINDENT,3,1))
*        eID_4 = VAL(SUBSTR(eINDENT,4,1))
*        eID_5 = VAL(SUBSTR(eINDENT,5,1))
*        eID_6 = VAL(SUBSTR(eINDENT,6,1))
*        eID_7 = VAL(SUBSTR(eINDENT,7,1))
*        eID_7 = VAL(SUBSTR(eNUM_PARC,1,1))

*       eID_1 = VAL(SUBSTR(eINDENT,1,1))
        eID_1 = VAL(SUBSTR(eINDENT,2,1))
        eID_2 = VAL(SUBSTR(eINDENT,3,1))
        eID_3 = VAL(SUBSTR(eINDENT,4,1))
        eID_4 = VAL(SUBSTR(eINDENT,5,1))
        eID_5 = VAL(SUBSTR(eINDENT,6,1))
        eID_6 = VAL(SUBSTR(eINDENT,7,1))
        eID_7 = VAL(SUBSTR(ALLTRIM(eNUM_PARC),1,1))

        eAG_1 = eID_1 * 80
        eAG_2 = eID_2 * 70
        eAG_3 = eID_3 * 60
        eAG_4 = eID_4 * 50
        eAG_5 = eID_5 * 40
        eAG_6 = eID_6 * 30
        eAG_7 = eID_7 * 20

        eDV   = eAG_1 + eAG_2 +eAG_3 + eAG_4 + eAG_5 + eAG_6 +eAG_7
        eDV   = IIF(MOD(eDV,11) = 10,0,MOD(eDV,11))
        eDV   = ALLTRIM(STR(eDV,5))

        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC

        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "101"+sisCGC_CPF+SPACE(10)+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,7)+eINDENT+eESP_IDENT+SPACE(12)+SUBSTR(eINDENT,2,6)+SUBSTR(ALLTRIM(eNUM_PARC),1,1)+eDV+;
        SPACE(25)+"101"+"00"+eINDENT+eDT_VENC+eVLR_PARCE+"291"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+ePRAZO_DEV+ePRAZO_PRO+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        REPLICATE(" ",43)+eREG_SEQ
   CASE eNUM_BANCO = "341"
        DO CASE
        CASE eCOD_CART = "112"
           eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
           eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
           * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
           IF eMSG_AUTO
              eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
           ENDIF
           ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(8)+eINDENT+eESP_IDENT+eCOD_BANCO+REPLICATE("0",13)+;
           "112"+SPACE(21)+"I"+"01"+eINDENT+eDT_VENC+eVLR_PARCE+"341"+SPACE(5)+"06"+"A"+;
           eDT_EMIS+IIF(PROTESTO,"09","92")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
           SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
           REPLICATE(" ",34)+eDT_VENC+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+" "+eREG_SEQ
        CASE eCOD_CART = "121"
           eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
           eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
           * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
           IF eMSG_AUTO
              eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
           ENDIF
           ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(25)+eINDENT+REPLICATE("0",8)+REPLICATE("0",13)+eCOD_BANCO+;
           "121"+SPACE(21)+"I"+"01"+"00"+eINDENT+eDT_VENC+eVLR_PARCE+"341"+SPACE(5)+"06"+"A"+;
           eDT_EMIS+IIF(PROTESTO,"09","92")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
           SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
           REPLICATE(" ",34)+eDT_VENC+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+" "+eREG_SEQ
        CASE eCOD_CART = "175" .OR. "179"
           eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
           eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
           * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
           IF eMSG_AUTO
              eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
           ENDIF
           * CALCULO DO DAC
           eAG_1 = VAL(SUBSTR(eCOD_AGENCI,1,1))
           eAG_2 = VAL(SUBSTR(eCOD_AGENCI,2,1))
           eAG_3 = VAL(SUBSTR(eCOD_AGENCI,3,1))
           eAG_4 = VAL(SUBSTR(eCOD_AGENCI,4,1))
           eCC_1 = VAL(SUBSTR(eCOD_CC,4,1))
           eCC_2 = VAL(SUBSTR(eCOD_CC,5,1))
           eCC_3 = VAL(SUBSTR(eCOD_CC,6,1))
           eCC_4 = VAL(SUBSTR(eCOD_CC,7,1))
           eCC_5 = VAL(SUBSTR(eCOD_CC,8,1))
           eCA_1 = 1
           eCA_2 = 7
           eCA_3 = 5
           eID_1 = VAL(SUBSTR(eINDENT,1,1))
           eID_2 = VAL(SUBSTR(eINDENT,2,1))
           eID_3 = VAL(SUBSTR(eINDENT,3,1))
           eID_4 = VAL(SUBSTR(eINDENT,4,1))
           eID_5 = VAL(SUBSTR(eINDENT,5,1))
           eID_6 = VAL(SUBSTR(eINDENT,6,1))
           eID_7 = VAL(SUBSTR(eINDENT,7,1))
           eID_8 = VAL(SUBSTR(eINDENT,8,1))

           eAG_1 = eAG_1 * 1
           eAG_2 = eAG_2 * 2
           eAG_3 = eAG_3 * 1
           eAG_4 = eAG_4 * 2
           eCC_1 = eCC_1 * 1
           eCC_2 = eCC_2 * 2
           eCC_3 = eCC_3 * 1
           eCC_4 = eCC_4 * 2
           eCC_5 = eCC_5 * 1
           eCA_1 = eCA_1 * 2
           eCA_2 = eCA_2 * 1
           eCA_3 = eCA_3 * 2
           eID_1 = eID_1 * 1
           eID_2 = eID_2 * 2
           eID_3 = eID_3 * 1
           eID_4 = eID_4 * 2
           eID_5 = eID_5 * 1
           eID_6 = eID_6 * 2
           eID_7 = eID_7 * 1
           eID_8 = eID_8 * 2

*?eCOD_AGENCI+"-"+SUBSTR(eCOD_CC,4,5)+"-"+"175"+"-"+eINDENT
*?
*?  eAG_1
*?  eAG_2
*?  eAG_3
*?  eAG_4
*?  eCC_1
*?  eCC_2
*?  eCC_3
*?  eCC_4
*?  eCC_5
*?  eCA_1
*?  eCA_2
*?  eCA_3
*?  eID_1
*?  eID_2
*?  eID_3
*?  eID_4
*?  eID_5
*?  eID_6
*?  eID_7
*?  eID_8

           eTAM_NUM = LENNUM(eAG_1)
*          ? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_1,eTAM_NUM,0)
*             ? e1_NUM
              e2_NUM = STR(eAG_1,eTAM_NUM,0)
*             ? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_1  = e1_NUM + e2_NUM
              *? eAG_1
              *?
           ENDIF
           eTAM_NUM = LENNUM(eAG_2)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_2,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eAG_2,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_2  = e1_NUM + e2_NUM
              *? eAG_2
              *?
           ENDIF
           eTAM_NUM = LENNUM(eAG_3)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_3,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eAG_3,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_3  = e1_NUM + e2_NUM
              *? eAG_3
              *?
           ENDIF
           eTAM_NUM = LENNUM(eAG_4)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_4,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eAG_4,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_4  = e1_NUM + e2_NUM
              *? eAG_4
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_1)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_1,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_1,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_1  = e1_NUM + e2_NUM
              *? eCC_1
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_2)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_2,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_2,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_2  = e1_NUM + e2_NUM
              *? eCC_2
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_3)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_3,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_3,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_3  = e1_NUM + e2_NUM
              *? eCC_3
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_4)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_4,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_4,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_4  = e1_NUM + e2_NUM
              *? eCC_4
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_5)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_5,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_5,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_5  = e1_NUM + e2_NUM
              *? eCC_5
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCA_1)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCA_1,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCA_1,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCA_1  = e1_NUM + e2_NUM
              *? eCA_1
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCA_2)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCA_2,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCA_2,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCA_2  = e1_NUM + e2_NUM
              *? eCA_2
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCA_3)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCA_3,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCA_3,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCA_3  = e1_NUM + e2_NUM
              *? eCA_3
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_1)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_1,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_1,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_1  = e1_NUM + e2_NUM
              *? eID_1
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_2)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_2,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_2,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_2  = e1_NUM + e2_NUM
              *? eID_2
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_3)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_3,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_3,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_3  = e1_NUM + e2_NUM
              *? eID_3
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_4)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_4,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_4,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_4  = e1_NUM + e2_NUM
              *? eID_4
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_5)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_5,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_5,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_5  = e1_NUM + e2_NUM
              *? eID_5
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_6)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_6,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_6,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_6  = e1_NUM + e2_NUM
              *? eID_6
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_7)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_7,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_7,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_7  = e1_NUM + e2_NUM
              *? eID_7
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_8)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_8,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_8,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_8  = e1_NUM + e2_NUM
              *? eID_8
              *?
           ENDIF

*?  eAG_1
*?  eAG_2
*?  eAG_3
*?  eAG_4
*?  eCC_1
*?  eCC_2
*?  eCC_3
*?  eCC_4
*?  eCC_5
*?  eCA_1
*?  eCA_2
*?  eCA_3
*?  eID_1
*?  eID_2
*?  eID_3
*?  eID_4
*?  eID_5
*?  eID_6
*?  eID_7
*?  eID_8

           eRESULTADO = 0
           eRESULTADO = eAG_1+eAG_2+eAG_3+eAG_4+eCC_1+eCC_2+eCC_3+eCC_4+eCC_5+eCA_1+eCA_2+eCA_3+eID_1+eID_2+eID_3+eID_4+eID_5+eID_6+eID_7+eID_8
*?eRESULTADO
           eDIVISAO   = 0
           eDIVISAO   = MOD(eRESULTADO,10)
*?eDIVISAO
           eDAC       = 0
           eDAC       = STR(((10 - eDIVISAO)-1),1,1)
*?eDAC


           ePAGAVEL = "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO              "

           ? "61"+eCOD_AGENCI+eCOD_CC+eCOD_CART+eINDENT+eDAC+"0"+SPACE(4)+eVLR_PARCE+eINDENT+"00"+eDT_VENC+"01A"+;
           eDT_EMIS+eTIPO_SAC+"0"+eCPF_CGC+SUBSTR(eCLIENTE,1,39)+SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+;
           eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+SUBSTR(eCLIENTE,1,30)+SPACE(4)+;
           ePAGAVEL+SPACE(55)+"02"+sisCGC_CPF+SPACE(31)+eREG_SEQ

           * Registros de Mensagens
           eNUM_SEQ = eNUM_SEQ + 1
           eTAM_SEQ = LENNUM(eNUM_SEQ)
           eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)

           ? "62"+eMSG+SPACE(29)+eMSG_2+SPACE(29)+eMSG_3+SPACE(29)+eMSG_4+SPACE(29)+eMSG_5+SPACE(29)+SPACE(47)+eREG_SEQ

        OTHERWISE
           SET PRINT OFF
           CLEAR
           PMENSAG("Codigo de carteira incompativel, carteira 112,121 ou 175 aceitos"
           PPARA()
           PLQUA("01")
           PMENSAG("Se sua carteira de cobranca e diferente destas, comunique a nossa Empresa."
           PPARA()
           GOTO BOTTOM
           SET PRINT ON
        ENDCASE
   CASE eNUM_BANCO = "409"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,4)+SUBSTR(eCOD_CC,1,7)+SPACE(9)+eESP_IDENT+eINDENT+REPLICATE("0",11)+;
        SUBSTR(eMSG,1,30)+SPACE(4)+SUBSTR(eCOD_CART,2,1)+"01"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"409"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+"03"+IF(PROTESTO,"06","07")+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;                    
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        REPLICATE(" ",40)+ePRAZO_PRO+"0"+eREG_SEQ
   CASE eNUM_BANCO = "422"

        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(6)+eESP_IDENT+eINDENT+SPACE(39)+;
        "000"+SPACE(1)+IIF(PROTESTO,ePRAZO_PRO,"00")+"101"+eINDENT+"00"+;
        eDT_VENC+eVLR_PARCE+"422"+SPACE(5)+"01"+"A"+eDT_EMIS+IIF(PROTESTO,"06","03")+;
        IIF(PROTESTO,"10","00")+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,10)+"  "+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,28)+"  "+SPACE(10)+SUBSTR(eNUM_ARQ,3,5)+eREG_SEQ
   CASE eNUM_BANCO = "424"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(9)+eESP_IDENT+eINDENT+SPACE(45)+;
        "901"+SPACE(2)+eINDENT+eDT_VENC+eVLR_PARCE+"424"+SPACE(5)+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"06","02")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        REPLICATE(" ",40)+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+" "+eREG_SEQ
   CASE eNUM_BANCO = "641"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+"000"+eCOD_CART+eCOD_AGENCI+eCOD_CC+eESP_IDENT+eINDENT+REPLICATE("0",20)+SPACE(25)+;
        "501"+SPACE(2)+eINDENT+eDT_VENC+eVLR_PARCE+"0000000001N"+eDT_EMIS+IIF(PROTESTO,"17","16")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+SUBSTR(eMSG,1,40)+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+"9"+eREG_SEQ
ENDCASE
eNUM_SEQ = eNUM_SEQ + 1
RETURN nil 
*************************  fim  *********************************
