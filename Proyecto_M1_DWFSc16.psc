
//Tarea Proyecto Modulo 1
//Natacha Rojas
//DWFS#16  (10-08-2024)
SubProceso Espacios <-GenerarCadena(num)   //Subproceso para generar espacios en blanco segun parametro num
	Definir Espacios como Cadena;
	Definir i como Entero;
	Espacios<-"";
	Para i<-1 Hasta num Hacer
		Espacios<-Concatenar(Espacios," ");		
	FinPara
FinSubProceso

Proceso Proyecto_M1
	Definir Cantidad Como Entero;
	Definir PrecioU como Entero;
	Definir Producto Como Caracter;
	Definir SubTotal Como Real;
	Definir Dcto Como Real;
	Definir DctoCupon como Real;
	Definir Pago Como Entero;
	Definir IVA como Real;
	Definir SiDescto Como Caracter;
	Definir Peso como Real;
	Definir Impuesto como Real;
	IVA<-19;  //constante % impuesto 2024	
	Definir D30 como Real;
	Definir D20 como Real;
	Definir D10 como Real;
	Definir LargoTexto Como Entero;
	Definir Zona Como Entero;
	Definir Vflete como Real;
	Definir Dd como Real;
	Definir VdctoCupon como Real;
	Definir Fletes Como Entero;
	Definir Formato Como Caracter;
	d30<-0.3;  //constante % Descuento 30%
	d20<-0.2;  //constante % Descuento 20%
	d10<-0.1;  //constante % Descuento 10%
	
	//Usar arreglo bidimensional para simular una BD ******************************
	Dimension Fletes[3,2];
	
	Fletes[1,1]<-3;  //Valor x kilo zona 1 (Norte)
	Fletes[2,1]<-2;  //Valor x kilo zona 2 (Centro)
	Fletes[3,1]<-3;  //Valor x kilo zona 3 (Sur)
	Fletes[1,2]<-500;  //Valor Fijo zona 1 (Norte)
	Fletes[2,2]<-200;  //Valor Fijo zona 2 (Centro)
	Fletes[3,2]<-500;  //Valor Fijo zona 3 (Sur)
	//****************************************************************************
	Definir SubTotalsd Como Real;
	DctoCupon<-0;
	Escribir "Ingrese Nombre Producto";
	Leer Producto;
	Escribir "Ingrese Cantidad Producto";
	Leer Cantidad;
	Escribir "Ingrese Precio Unitario Producto";
	Leer PrecioU;
	Escribir "Tiene Cupón de Descuento? (S/N)";
	Leer SiDescto;
	si SiDescto=="S" o SiDescto=="s" entonces
		Repetir
			Escribir "Ingrese % Cupon Descuento (1-100)";
			Leer DctoCupon;
		Hasta Que DctoCupon>0 y DctoCupon<=100
	FinSi
	Escribir "Ingrese Peso Producto Unitario en Kilos";
	Leer Peso;
	Repetir
		Escribir "Ingrese Zona Despacho [1=Norte, 2=Centro, 3=Sur]";
		Leer Zona;
	Hasta Que Zona==1 o Zona==2 o Zona==3	
	
	//Formulas *********************************************
	Dcto<-0;
	dd<-0;
	SubTotal<-Cantidad*PrecioU;
	
	SubTotalsd<-SubTotal;  //Guardo Valor Inicial Neto
	VdctoCupon<-(SubTotal*(DctoCupon/100));
	SubTotal<-SubTotal-(SubTotal*(DctoCupon/100));  //Aplico Cupon de descuento
	Impuesto<-SubTotal*(Iva/100);  //calculo impuesto al valor inicial menos el descuento
	SubTotal<-SubTotal+Impuesto;
	//Aplico Descuento x Cantidad
	si Cantidad>=3 y Cantidad<20 Entonces
		Dcto<-SubTotal*d10;
		dd<-(d10*100);
	SiNo
		si Cantidad>=20 y Cantidad<50 Entonces
			Dcto<-SubTotal*d20;
			dd<-(d20*100);
		SiNo
			si Cantidad>=50 Entonces
				Dcto<-SubTotal*d30;
				dd<-(d30*100);
			FinSi
		FinSi
	FinSi
	
	Vflete<-Fletes[Zona,2]+((Peso*Cantidad)*Fletes[Zona,1]);
	SubTotal<-(SubTotal-Dcto)+Vflete;
	//************************* Resultados **************************************************************	
	Escribir "****************************  Resumen Compra *********************************************";
	Escribir "==========================================================================================";
	//Escribir "Valor Final Producto: $", SubTotal;
	Escribir "Producto: ", Mayusculas(Producto), ", Cantidad: ", Cantidad, ", Precio Unitario: ", PrecioU;
	si VdctoCupon<>0 entonces
		LargoTexto<-Longitud(Concatenar("Cupon Descuento ", ConvertirATexto(DctoCupon)))+Longitud(Concatenar("%:-$", ConvertirATexto(VdctoCupon)));
		Formato<-GenerarCadena(49-LargoTexto);
		Escribir "Cupon Descuento ", DctoCupon,"%:", Formato, "-$", VdctoCupon;
	finsi 
	
	LargoTexto<-Longitud(Concatenar("Impuesto ",ConvertirATexto(IVA)))+Longitud(Concatenar("%: $",ConvertirATexto(Impuesto)));
	Formato<-GenerarCadena(50-LargoTexto);
	Escribir "Impuesto ", IVA,"%:", Formato, "$", Impuesto;
	si Dcto>0 entonces
		LargoTexto<-Longitud(Concatenar("Descuento por volumen (", ConvertirATexto(dd)))+Longitud(Concatenar("%): -$", ConvertirATexto(Dcto)));
		Formato<-GenerarCadena(50-LargoTexto);
		Escribir "Descuento por volumen (",dd,"%):", Formato, "-$", Dcto;
	fin si
	
	LargoTexto<-Longitud(Concatenar("Valor Flete:$",ConvertirATexto(Vflete)));
	Formato<-GenerarCadena(49-LargoTexto);
	Escribir "Valor Flete:", Formato, "$", Vflete;
	
	LargoTexto<-Longitud(Concatenar("Total a Pagar:$",ConvertirATexto(SubTotal)));
	Formato<-GenerarCadena(49-LargoTexto);
	Escribir "Total a Pagar:", Formato, "$", SubTotal;	
	Escribir "==========================================================================================";
	Escribir "********************************Gracias Por Su Compra ************************************";
		
	
FinProceso
