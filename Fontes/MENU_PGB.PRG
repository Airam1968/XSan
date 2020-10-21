*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_PGB.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE PAGAMENTOS BALCAO
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
************************* inicio ************************************
SAVE SCREEN TO TELA03
DO PLQUA WITH '01'
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   DO PTIT WITH 'MENU MANUTENÄ«O DE PARCELAS'
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,50 CLEAR TO 20,70
   @ 09,50 TO 20,70
   @ 10,51 PROMPT ' 1  Pgto. Unidade  ' MESSAGE ' Pagamento de Parcelas por Unidade              '
   @ 11,51 PROMPT ' 2  Pgto. Propriet.' MESSAGE ' Pagamento de Parcelas por Propriet†rio         '
   @ 12,51 PROMPT ' 3   Cancelamento  ' MESSAGE ' Cancelamento de Pagamento Indevido             '
   @ 13,51 PROMPT ' 4    Desmembrar   ' MESSAGE ' Desmembramento de Uma Parcela em Duas          '
   @ 14,51 PROMPT ' 5   Nova Parcela  ' MESSAGE ' Criaá∆o de Parcelas                            '
   @ 15,51 PROMPT ' 6 Excluir Parcela ' MESSAGE ' Exclus∆o de Parcelas                           '
   @ 16,51 PROMPT ' 7 Mudar Vencimento' MESSAGE ' Alteraá∆o das Data de Vencimento no Per°odo    '
   @ 17,51 PROMPT ' 8 Mudar Cobranáa  ' MESSAGE ' Alteraá∆o da Cobranáa em Carteira ou Banc†ria  '
   @ 18,51 PROMPT ' 9    Protesto     ' MESSAGE ' Habilita ou Desabilita Instruá∆o de Protesto   '
   @ 19,51 PROMPT ' Esc     Fim       ' MESSAGE ' Fim de Operaá∆o do M¢dulo                      '
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA03
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO('DI_CA_03')
	    DO CAD_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
           F_LACESSO(sisUSUARIO,'CADASTRO DE PAGAMENTOS UNIDADE','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO('DI_CA_03')
           if sisCGC_CPF <> '00915210000100'
             DO CAD_PAGP
           else
		     *Terra Sim∆o
			 DO CAD_PAG2           
           endif
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
           F_LACESSO(sisUSUARIO,'CADASTRO DE PAGAMENTOS PROPRIETARIO','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO('DI_EX_03')
	   DO EXC_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
	   F_LACESSO(sisUSUARIO,'EXCLUSAO DE PAGAMENTOS BALCAO','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 4
        IF F_ACESSO('DI_AL_03')
	   DO DES_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
	   F_LACESSO(sisUSUARIO,'DESMEMBRAMENTO DE PAGAMENTOS','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 5
        IF F_ACESSO('DI_CA_03')
	   DO NOV_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
	   F_LACESSO(sisUSUARIO,'NOVA PARCELA','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 6
        IF F_ACESSO('DI_AL_03')
           DO CAN_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
           F_LACESSO(sisUSUARIO,'EXCLUSAO DE PARCELAS','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 7
        IF F_ACESSO('DI_AL_03')
	   DO VEN_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
	   F_LACESSO(sisUSUARIO,'ALTERACAO DE VENCIMENTOS','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 8
        IF F_ACESSO('DI_AL_03')
	   DO COB_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
	   F_LACESSO(sisUSUARIO,'ALTERACAO PRACA COBRANCA','ACESSO NEGADO')        
	ENDIF
     CASE sisOP = 9
        IF F_ACESSO('DI_AL_03')
	   DO PRO_PAGB
	ELSE
	   DO PLQUA WITH '01'
	   DO PMSGR WITH 'Acesso Negado !',1
	   F_LACESSO(sisUSUARIO,'INSTRUCAO DE PROTESTO PAGAMENTOS','ACESSO NEGADO')        
	ENDIF
     CASE LASTKEY() = 27
	RESTORE SCREEN FROM TELA03
	CLOSE DATABASES
	RETURN
     ENDCASE
ENDDO
*************************  fim  *********************************
