program Vapor

!-------------------------------------------------------------
! por Pedro Miguel Portela Miranda
! portela.miranda@gmail.com
!
!Calcula a densidade de vapor e a humidade relativa a partir da 
!temperatura do ar e de um dos seguintes parâmetros: 
!temperatura do ponto de orvalho ou humidade relativa.
! as variáveis são
! Temp - temperatura do ar
! DewP - temperatura do ponto de orvalho
! FrostP - temperatura do ponto de geada
! pvap - Pressão de vapor de água
! ev_w - função que obtem a pressão de saturação de vapor de água em função da temperatura, em relação à água líquida
! Td - função que obtem o ponto de orvalho em função da pressão de vapor de água
! ev_i - função que obtem a pressão de saturação vapor de água em função da temperatura, em relação ao gelo
! Tf - função que obtem o ponto de geada em função da pressão de vapor de água
! rho_vap - Densidade de vapor
! pvap_s_w - Pressão de vapor de água de saturação, em relação à água líquida
! pvap_s_i - Pressão de vapor de água de saturação, em relação ao gelo
! RH - Humidade relativa
! RH_i - Humidade relativa, em relação ao gelo
! posicao - posicao do simbolo "%" na variável introduzida (humidade)
!            também serve para verificar se existe "%"
! recomeco - se igual a "r", o programa volta ao inicio
! humidade - temperatura do ponto de orvalho, ou humidade relativa se com "%"
! forma - formato para leitura da humidade se for em humidade relativa
!         lendo até ao caracter anterior ao "%"
!------------------------------------------------------------



implicit none

real :: Temp, DewP, FrostP, pvap, ev_w, Td, ev_i, Tf, rho_vap, pvap_s_w, pvap_s_i, RH, RH_i
integer :: posicao_h, posicao_p
character(1) :: recomeco
character(16) :: humidade
character(40) :: forma

print *
print *, "####################################################################"
print *
print *, " DENSIVAP v.1.4.0 - 30 DEZ 2015 "
print *
print *, " CALCULADOR DE DENSIDADE DO VAPOR DE AGUA"
print * 

do
	print *, "####################################################################"
	print *
	! as 4 seguintes condições obrigam à entrada no loop que evita a introdução errada de dados
	Temp = -301
	DewP = -300
	RH = -50
	pvap = -1

	! -> ESTE loop! (Temp < DewP) .OR. .OR. (RH > 100) 
	do while ((RH < 0) .OR. (pvap < 0) .OR. (Temp > 199) .OR. (DewP > 199) .OR. (Temp < -100) .OR. (DewP < -100)) 
	
		write (*,'(1x, A)', advance="no") "Introduza temperatura do ar (em graus celsius): "
		read *, Temp

		print *

		write (*,'(1x, A)') "Introduza temperatura do ponto de orvalho (em graus celsius)" 
		write (*,'(1x, A)') "ou a humidade relativa (em % utilizando o simbolo %)"
		write (*,'(1x, A)', advance="no") "ou a pressão de vapor de água (em Pa, utilizando o sufixo 'p'): "
		read *, humidade
	
		!  O seguinte serve para detectar (ou não) o simbolo % e indicar a sua posição
		! se for 0, então não exite "%" e trata-se da Temperatura do ponto de orvalho
		! .false.' serve para não procurar de trás para a frente. poderia ser omitido.
		posicao_h = scan (humidade, "%" , .false.)
		posicao_p = scan (humidade, "p" , .false.)
		if (posicao_h /= 0) then !trata-se de humidade relativa
			!o seguinte cria o formato que lê um número real com (posição-1) algarimsos ou seja até ao anterior à "%"
			write (forma, *), "(F", posicao_h-1 , ".0)" 
			!o seguinte lê "RH" (real) na variável "humidade" (character) que foi introduzida pelo utilizador com %
			read (humidade,forma) RH
			DewP = Temp ! -> serve so para sair do loop, mais tarde vai ser bem calculada
			pvap=1 !-> serve so para sair do loop, mais tarde vai ser bem calculada
		end if
		
		if (posicao_p /= 0) then !trata-se de pressão de vapor
			!o seguinte cria o formato que lê um número real com (posição-1) algarimsos ou seja até ao anterior à letra "p"
			write (forma, *), "(F", posicao_p-1 , ".0)" 
			!o seguinte lê "pvap" (real) na variável "humidade" (character) que foi introduzida pelo utilizador com Pa
			read (humidade,forma) pvap
			DewP = Temp ! -> serve so para sair do loop, mais tarde vai ser bem calculada
			RH=1 ! -> serve so para sair do loop, mais tarde vai ser bem calculada
		end if

		if ((posicao_p == 0) .and. (posicao_h == 0)) then
			read (humidade,*), DewP
			RH=1 ! -> serve so para sair do loop, mais tarde vai ser bem calculada
			pvap=1 ! -> serve so para sair do loop, mais tarde vai ser bem calculada
		end if
	
	
		if (Temp < DewP) then
		print *
		print *, "** AVISO ** Normalmente o ponto de orvalho não é superior à temperatura do ar!"
		print *
		read *
		end if
			
		if (RH < 0) then
		print *
		print *, "** ERRO ** A humidade relativa nao pode ser inferior a 0%!"
		print *
		end if
			
		if (RH > 100) then
		print *
		print *,  "** AVISO ** A humidade relativa normalmente não é superior a 100%!"
		print *
		read *
		end if

		if (pvap < 0) then
		print *
		print *, "** ERRO ** A pressao de vapor nao pode ser negativa!"
		print *
		end if

		if ((Temp > 199) .OR. (DewP > 199) .OR. (Temp < -100) .OR. (DewP < -100)) then
		print *
		print *, "** ERRO ** Temperatura fora dos limites!"
		print *
		end if

	end do

		! Nos cálculos seguintes soma-se 273.15 para converter em kelvin
		! a densidade é multiplicada por 1000 para obter g/cm^3 a partir de kg/m^3
		! 461.5 é a constante especifica do vapor de água na eq dos gases (R)
		! rho= P/(R.T) 

	if ((posicao_p == 0) .and. (posicao_h == 0)) then ! humidade dada pelo ponto de orvalho

	
		Temp = Temp + 273.15
		DewP = DewP + 273.15
		pvap = ev_w(DewP)
		pvap_s_w = ev_w(Temp)
		rho_vap = 1000.0 * pvap / (461.5 * (Temp))
		RH = pvap / pvap_s_w * 100.0
	end if
	
	if (posicao_h /= 0) then ! humidade dada pela humidade relativa
		Temp = Temp + 273.15
		pvap_s_w = ev_w(Temp)
		pvap = pvap_s_w * RH / 100.0
		rho_vap = 1000.0 * pvap / (461.5 * (Temp))
		DewP = Td(pvap)
	end if

	if (posicao_p/= 0) then ! humidade dada pela pressão de vapor
		Temp = Temp + 273.15
		DewP = Td(pvap)
		pvap_s_w = ev_w(Temp)
		rho_vap = 1000.0 * pvap / (461.5 * (Temp))
		RH = pvap / pvap_s_w * 100.0
	end if

	! Se a temperatura for negativa, calcula-se a temperatura do ponto de geada, a pressão de vapor de saturação em relação ao gelo,
	! e a humidade relativa em relação ao gelo
	if ((Temp < 273.16) .or. (Temp == 273.16)) then
		pvap_s_i = ev_i(Temp)
		RH_i = pvap / pvap_s_i * 100.0
		FrostP = Tf(pvap)
	end if
		
	

	print *, 
	print *, "************************************************************"
	print *,
	print '(1x, "Temperatura:", F5.1, " graus celsius")', (Temp - 273.15)
	print '(1x, "Ponto de orvalho:", F5.1, " graus celsius")', (DewP - 273.15)
	print '(1x, "Humidade relativa:", F5.1, "%")', RH
	print '(1x, "Densidade do vapor de agua:", F5.1, " grama por metro cubico")', rho_vap
	write (*,'(1x,A,F10.2,A)') "Pressão de vapor de agua:", pvap, "Pa"
	write (*,'(1x,A,F10.2,A)') "Pressão de saturação de vapor de agua:", pvap_s_w, "Pa"
	print *

	if ((Temp < 273.16) .or. (Temp == 273.16)) then
		print '(1x, "Ponto de geada:", F5.1, " graus celsius")', (FrostP - 273.15)
		print '(1x, "Humidade relativa, relativamente ao GELO:", F5.1, "%")', RH_i
		write (*,'(1x,A,F10.2,A)') "Pressão de saturação de vapor de agua, relativamente ao GELO:", pvap_s_i, "Pa"
	end if

	print *, 
	print *, "************************************************************"
	print *, 
	print *, "Prima a tecla S para sair ou qualquer outra letra para recomecar,"
	print *, "e de seguida prima ENTER"
	read *, recomeco
	if (recomeco=="S" .or. recomeco=="s") exit
	
end do
end program


!------------------------------------------------------------------
function ev_w(T) ! função que calcula a pressão de vapor de saturação, em relação à água líquida, para uma certa temperatura
real, intent (in):: T
real ev_w

! A equação seguinte, da pressao de vapor de agua em funcao da temperatura, e' da OMM, baseada num paper de Goff (1957)
! é diferente da de Goff-Gratch (1946); T em keLvin e a multiplicação inicial por 100 é para dar em Pa (e não em hPa)

ev_w = 100.0*10.0**(10.79574*(1-273.16/T)-5.028*(log10(T/273.16)) &
+1.50475e-4*(1-10.0**(-8.2969*(T/273.16-1.0)))+ &
0.42873e-3*(10.0**(4.76955*(1-273.16/T))-1.0)+0.78614)

return

end function

!------------------------------------------------------------------
function Td(WVP) ! função que calcula a temperatura do ponto de orvalho com base na pressão de vapor
real, intent (in):: WVP
real WVP_temp,T,Tdmax,Tdmin,Td

Tdmax=473.15 ! máxima temperatura do intervalo para iteração, para achar ponto de orvalho em função da pressão de vapor
Tdmin=073.15 ! mínima temperatura do intervalo para iteração

do while (abs(Tdmax-Tdmin)>0.01) ! enquanto o intervalo for superior a 0.01K, continua-se a refinar o intervalo
	
	T = (Tdmin+Tdmax) / 2 ! assume-se que a temperatura é a do meio do intervalo
! serviu para debug ******** print *, wvp, wvp_temp, Tdmin, t,Tdmax

	! A equação seguinte, da pressao de vapor de agua em funcao da temperatura, e' da OMM, baseada num paper de Goff (1957)
	! é diferente da de Goff-Gratch (1946); T em keLvin e a multiplicação inicial por 100 é para dar em Pa (e não em hPa)

	WVP_temp = 100.0*10.0**(10.79574*(1-273.16/T)-5.028*((log(T/273.16))/(log(10.0)) ) &
	+1.50475e-4*(1-10.0**(-8.2969*(T/273.16-1.0)))+ &
	0.42873e-3*(10.0**(4.76955*(1-273.16/T))-1.0)+0.78614)

	! os seguintes servem para estreitar o intervalo de temperaturas

	if (WVP > WVP_temp) then
	Tdmin=T
	
	end if

	if (WVP < WVP_temp) then
	Tdmax=T
	
	end if

end do
	
Td=T

return

end function

!------------------------------------------------------------------
function ev_i(T) ! função que calcula a pressão de vapor de saturação em relação ao gelo, para uma certa temperatura
real, intent (in):: T
real ev_i

! A equação seguinte, da pressao de vapor de agua, relativamente ao GELO, em funcao da temperatura, e' da OMM, baseada num paper de Goff (1957)
! é diferente da de Goff-Gratch (1946); T em kelvin e a multiplicação inicial por 100 é para dar em Pa (e não em hPa)
ev_i = 100.0*10.0**(-9.09718*(273.16/T-1)-3.56654*(log10(273.16/T))+0.876793*(1-T/273.16)+(log10(6.1071)))

return

end function

!------------------------------------------------------------------
function Tf(WVP) ! função que calcula a temperatura do ponto de geada com base na pressão de vapor
real, intent (in):: WVP
real WVP_temp_i,T,Tfmax,Tfmin,Tf

Tfmax=273.16 ! máxima temperatura do intervalo para iteração, para achar ponto de geada em função da pressão de vapor
Tfmin=073.15 ! mínima temperatura do intervalo para iteração

do while (abs(Tfmax-Tfmin)>0.01) ! enquanto o intervalo for superior a 0.01K, continua-se a refinar o intervalo
	
	T = (Tfmin+Tfmax) / 2 ! assume-se que a temperatura é a do meio do intervalo
	! serviu para debug ******** print *, wvp, wvp_temp, Tfmin, T ,Tfmax

	! A equação seguinte, da pressao de vapor de agua, relativamente ao GELO, em funcao da temperatura, 
	! e' da OMM, baseada num paper de Goff (1957)
	! é diferente da de Goff-Gratch (1946); T em kelvin e a multiplicação inicial por 100 é para dar em Pa (e não em hPa)

	WVP_temp_i = 100.0*10.0**(-9.09718*(273.16/T-1)-3.56654*(log10(273.16/T))+0.876793*(1-T/273.16)+(log10(6.1071)))

	! os seguintes servem para estreitar o intervalo de temperaturas

	if (WVP > WVP_temp_i) then
	Tfmin=T
	
	end if

	if (WVP < WVP_temp_i) then
	Tfmax=T
	
	end if

end do
	
Tf=T

return

end function
