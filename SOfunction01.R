#SOFunction01.R  #############################
one_sim = function( R, C, BT02 , DFFS, RPEM1, Cpool){
	RC <- R*C  #園内本数

	#1 抽出プールからクローンを植栽本数回、抽出。
	DFFS$CloneID <- sample(Cpool, size=RC, replace=F)

	#2 計算に必要な特性データ(BT02)とクローンセットデータ(DFFS)を新たなdf(TDS:Total Data Set)に集約する
	TDS <- (merge(DFFS,BT02, by.x="CloneID", by.y="No",sort = F) )#ベーステーブルBT中のクローンの記載順序（No）と抽出クローンデータフレームのCloneIDをキーにして結合
	TDS <- TDS[ order(TDS$PPID), ]#データの並び替え

	#3 両親情報付きの近交係数ベクトル　Inbreeding Coefficient Vector with Parental information:ICVP
	ICVP = c(TDS$Clone,TDS$Math,TDS$Fath)
	ICVP <- factor(ICVP)
	ICVP <- as.numeric(ICVP)

	#4 両親情報付きの近交係数行列　Inbreeding Coefficient Matrix with Parental information
	ICMP = as.matrix(dist(ICVP,diag=t,upper=T))
	#ICMPを6つの行列に分ける	
	IIM = ICMP[1:RC,1:RC] #個体間行列Individual individual Matrix :IIM
	IMM = ICMP[1:RC,(RC+1):(2*RC)] #個体-母親行列Individual Mather Matrix: IMM
	IFM = ICMP[1:RC,(2*RC+1):(3*RC)] #個体-花粉親行列Individual Father Matrix: IFM
	MMM = ICMP[(RC+1):(2*RC),(RC+1):(2*RC)] #母親-母親行列Mother Mother Matrix: MMM
	MFM = ICMP[(RC+1):(2*RC),(2*RC+1):(3*RC)] #母親-花粉親行列Mother Father Matrix: MFM
	FFM = ICMP[(2*RC+1):(3*RC),(2*RC+1):(3*RC)] #花粉親-花粉親行列Father Father Matrix: FFM
		MMM <- MMM+diag(RC)#母親間行列の対角要素を0から1に変換しておく
		FFM <- FFM+diag(RC)#父親間行列の対角要素を0から1に変換しておく
	IIM = ifelse(IIM == 0, 0.5, 0)#血縁の強さに応じた近交係数を付与する，同クローンは0.5
	IMM = ifelse(IMM == 0, 0.25, 0)
	IFM = ifelse(IFM == 0, 0.25, 0)
	MMM = ifelse(MMM == 0, 0.125, 0)
	MFM = ifelse(MFM == 0, 0.125, 0)
	FFM = ifelse(FFM == 0, 0.125, 0)

	#5 近交係数行列（Inbreeding Coefficient Matrix : ICM）,系統重みづけ無の平均近交度(ACP:average coancestry of population)
	ICM = IIM+IMM+IFM+MMM+MFM+FFM #植栽木間，総当たりの近交行列
	ACP = round((sum(ICM))/(RC*RC),digits = 5) #距離で重み付けした近交係数の平均値
	WICM = ICM*RPEM1 #weighted ICM，近交行列を距離に基づく相対寄与率（一個体離れると1/2）で重みづけ
	WACP1 = round((sum(WICM))/(RC*RC),digits = 5) #距離で重み付けした近交係数の平均値
	#WICM2 = ICM*RPEM2 #weighted ICM，近交行列を距離に基づく相対寄与率(距離の自乗の反比例)で重みづけ
      #WACP2 = round((sum(WICM2))/(RC*RC),digits = 5)

	#6 結果を返す　平均重みづけ近交係数、クローン構成,距離で調整した総当たり個体間近交係数
      return(c(ACP,WACP1,DFFS$CloneID))
}