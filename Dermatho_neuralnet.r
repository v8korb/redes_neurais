library("neuralnet")
library("clusterSim")

setwd("C:/Users/korb/cadeiras/UFSCar/IRN/trabalho1/dermatho")

mydataTeste = read.csv("treino_dermatho_csv.csv", header=FALSE)

#mydataTeste = read.csv("Ecoli_teste_mod.csv")

#separando entrada de saída
#não pega a coluna 34, pois há valores faltantes
inputTeste <- mydataTeste[,1:33] # X
outputTeste <- mydataTeste[,34+1] # desejado

#Normalizando entre -1 e 1
	scaled = data.Normalization (inputTeste,type="n5",normalization="column")
	

#testa se falta valores
any(is.na(scaled))
any(is.na(outputTeste))

#normalizando saida
#inicializando matriz de desejado com zero
desejadoTeste <- matrix(ncol = 6, nrow = length(outputTeste), 0)

#colocando '1' no valor desejado da matriz
i=1
while(i <= length(outputTeste)){
desejadoTeste[i,outputTeste[i]] <- 1
i <- i + 1
}

#colnames(desejadoTeste, do.NULL=FALSE);
#colnames(desejadoTeste)[1] <- "s1"
#colnames(desejadoTeste)[2] <- "s2"
#colnames(desejadoTeste)[3] <- "s3"
#colnames(desejadoTeste)[4] <- "s4"
#colnames(desejadoTeste)[5] <- "s5"

desejadoTeste2=as.data.frame(desejadoTeste)
colnames(desejadoTeste2)[1] <- "s1"
colnames(desejadoTeste2)[2] <- "s2"
colnames(desejadoTeste2)[3] <- "s3"
colnames(desejadoTeste2)[4] <- "s4"
colnames(desejadoTeste2)[5] <- "s5"
colnames(desejadoTeste2)[6] <- "s6"


colnames(scaled)[1] <- "e1"
colnames(scaled)[2] <- "e2"
colnames(scaled)[3] <- "e3"
colnames(scaled)[4] <- "e4"
colnames(scaled)[5] <- "e5"
colnames(scaled)[6] <- "e6"
colnames(scaled)[7] <- "e7"
colnames(scaled)[8] <- "e8"
colnames(scaled)[9] <- "e9"
colnames(scaled)[10] <- "e10"
colnames(scaled)[11] <- "e11"
colnames(scaled)[12] <- "e12"
colnames(scaled)[13] <- "e13"
colnames(scaled)[14] <- "e14"
colnames(scaled)[15] <- "e15"
colnames(scaled)[16] <- "e16"
colnames(scaled)[17] <- "e17"
colnames(scaled)[18] <- "e18"
colnames(scaled)[19] <- "e19"
colnames(scaled)[20] <- "e20"
colnames(scaled)[21] <- "e21"
colnames(scaled)[22] <- "e22"
colnames(scaled)[23] <- "e23"
colnames(scaled)[24] <- "e24"
colnames(scaled)[25] <- "e25"
colnames(scaled)[26] <- "e26"
colnames(scaled)[27] <- "e27"
colnames(scaled)[28] <- "e28"
colnames(scaled)[29] <- "e29"
colnames(scaled)[30] <- "e30"
colnames(scaled)[31] <- "e31"
colnames(scaled)[32] <- "e32"
colnames(scaled)[33] <- "e33"


n=names(desejadoTeste2)

n2=names(scaled)


#formula
#f <- as.formula(paste(desejadoTeste2))
#f <- as.formula(paste("~", paste(n[!n %in% "train"], collapse = " + ")))

train = data.frame(scaled, desejadoTeste2)

#f <- as.formula(paste('n2~', paste(n[!n %in% 'inputTeste2'], collapse = ' + ')))
#net
#nn = neuralnet(f,data=train,hidden=c(5))
#nn = neuralnet(f,data=inputTeste2,hidden=c(5),size=5)
##adicionar: algorithm=backprop, threshold=0.0001, act.fct=logistic, err.fct=sse, rep=10, linear.ouptut=FALSE
nn1 = neuralnet(
	s1 + s2 + s3 + s4 + s5 + s6 ~ e1 + e2 + e3 + e4 + e5 + e6 + e7 + e8 + e9 + e10 + e11 + e12 + e13 + e14 + e15 + e16 + e17 + e18 + e19 + e20 + e21 + e22 + e23 + e24 + e25 + e26 + e27 + e28 + e29 + e30 + e31 + e32 + e33,
	data=train,
	hidden=c(13),
	learningrate=0.01,
	#threshold=0.001,
	#algorithm="backprop",
	#act.fct="logistic",
	#err.fct="ce",
	#stepmax=10000,
	rep=5,
	linear.output=FALSE,
	)

nn2 = neuralnet(
	s1 + s2 + s3 + s4 + s5 + s6 ~ e1 + e2 + e3 + e4 + e5 + e6 + e7 + e8 + e9 + e10 + e11 + e12 + e13 + e14 + e15 + e16 + e17 + e18 + e19 + e20 + e21 + e22 + e23 + e24 + e25 + e26 + e27 + e28 + e29 + e30 + e31 + e32 + e33,
	data=train,
	hidden=c(26),
	learningrate=0.01,
	#threshold=0.001,
	#algorithm="backprop",
	#act.fct="logistic",
	#err.fct="ce",
	#stepmax=10000,
	rep=5,
	linear.output=FALSE,
	)

nn3 = neuralnet(
	s1 + s2 + s3 + s4 + s5 + s6 ~ e1 + e2 + e3 + e4 + e5 + e6 + e7 + e8 + e9 + e10 + e11 + e12 + e13 + e14 + e15 + e16 + e17 + e18 + e19 + e20 + e21 + e22 + e23 + e24 + e25 + e26 + e27 + e28 + e29 + e30 + e31 + e32 + e33,
	data=train,
	hidden=c(39),
	learningrate=0.01,
	#threshold=0.001,
	#algorithm="backprop",
	#act.fct="logistic",
	#err.fct="ce",
	#stepmax=10000,
	rep=5,
	linear.output=FALSE,
	)	
	
mydata = read.csv("test_dermatho_csv.csv")

#separando entrada de saída
inputTeste <- mydata[,1:33] # X
outputTeste <- mydata[,34+1] # desejado

#Normalizando entre -1 e 1
	scaled = data.Normalization (inputTeste,type="n5",normalization="column")

#normalizando saida
#inicializando matriz de desejado com zero
desejadoTeste <- matrix(ncol = 6, nrow = length(outputTeste), 0)

#colocando '1' no valor desejado da matriz
i=1
while(i <= length(outputTeste)){
desejadoTeste[i,outputTeste[i]] <- 1
i <- i + 1
}

resp1t = compute(nn1,scaled)
resp2t = compute(nn2,scaled)
resp3t = compute(nn3,scaled)

#output2 = outputTeste

conf1 = matrix(nrow=6,ncol=6);
conf1 = apply(conf1,c(1,2), function(x) 0);

conf2 = matrix(nrow=6,ncol=6);
conf2 = apply(conf2,c(1,2), function(x) 0);

conf3 = matrix(nrow=6,ncol=6);
conf3 = apply(conf3,c(1,2), function(x) 0);

#monstando matriz confusao1
resp2 <- matrix(unlist(resp1t[2]), ncol = 6)
for (i in 1:91){
	tst=-10;
	jt=0;
	for(j in 1:6){
		if(resp2[i,j]>tst){
			tst=resp2[i,j]
			jt=j;
		}
		resp2[i,j]=0;
	}
	resp2[i,jt]=1;
}		

for (i in 1:91){
	tstR=-10;
	jtR=0;
	tstD=-10;
	jtD=0;
	for(j in 1:6){
		if(resp2[i,j]>tstR){
			tstR=resp2[i,j]
			jtR=j;
		}
		if(desejadoTeste[i,j]==1){
			jtD=j;
		}
	}
	conf1[jtD,jtR]=conf1[jtD,jtR]+1;
}	

acertos1=0;
	for(i in 1:6){
		acertos1=acertos1+conf1[i,i];
	}
	
#plot.nn(nn1,dimension=10)

#monstando matriz confusao2
resp2 <- matrix(unlist(resp2t[2]), ncol = 6)
for (i in 1:91){
	tst=-10;
	jt=0;
	for(j in 1:6){
		if(resp2[i,j]>tst){
			tst=resp2[i,j]
			jt=j;
		}
		resp2[i,j]=0;
	}
	resp2[i,jt]=1;
}		

for (i in 1:91){
	tstR=-10;
	jtR=0;
	tstD=-10;
	jtD=0;
	for(j in 1:6){
		if(resp2[i,j]>tstR){
			tstR=resp2[i,j]
			jtR=j;
		}
		if(desejadoTeste[i,j]==1){
			jtD=j;
		}
	}
	conf2[jtD,jtR]=conf2[jtD,jtR]+1;
}	

acertos2=0;
	for(i in 1:6){
		acertos2=acertos2+conf2[i,i];
	}
	
#plot.nn(nn2,dimension=10)

#monstando matriz confusao3
resp2 <- matrix(unlist(resp3t[2]), ncol = 6)
for (i in 1:91){
	tst=-10;
	jt=0;
	for(j in 1:6){
		if(resp2[i,j]>tst){
			tst=resp2[i,j]
			jt=j;
		}
		resp2[i,j]=0;
	}
	resp2[i,jt]=1;
}		

for (i in 1:91){
	tstR=-10;
	jtR=0;
	tstD=-10;
	jtD=0;
	for(j in 1:6){
		if(resp2[i,j]>tstR){
			tstR=resp2[i,j]
			jtR=j;
		}
		if(desejadoTeste[i,j]==1){
			jtD=j;
		}
	}
	conf3[jtD,jtR]=conf3[jtD,jtR]+1;
}	

acertos3=0;
	for(i in 1:6){
		acertos3=acertos3+conf3[i,i];
	}
	
#plot.nn(nn3,dimension=10)
	

	atrD = c(0,0,0,0,0,0)
	atrR = c(0,0,0,0,0,0)
for (i in 1:91){
	tstR=-10;
	jtR=0;
	for(j in 1:6){
		if(resp2[i,j]>tstR){
			tstR=resp2[i,j]
			jtR=j;
		}
	}
	atrR[jtR]=atrR[jtR]+1;
}
for (i in 1:274){
	tstD=-10;
	jtD=0;
	for(j in 1:6){
		if(desejadoTeste2[i,j]==1){
				jtD=j;
		}
	}
	atrD[jtD]=atrD[jtD]+1;
}

