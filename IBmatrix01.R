#IBmatrix01.R
IBfunc = function(CloneID,MathID,FathID){
    ICVP = c(CloneID,MathID,FathID)
    ICVP <- factor(ICVP)
    ICVP <- as.numeric(ICVP)
    ICMP = as.matrix(dist(ICVP,diag=t,upper=T))#ICMPを6つの行列に分ける	
    NC <- (length(ICVP))/3
    IIM = ICMP[1:NC,1:NC] #個体間行列Individual individual Matrix :IIM
    IMM = ICMP[1:NC,(NC+1):(2*NC)] #個体-母親行列Individual Mather Matrix: IMM
    IFM = ICMP[1:NC,(2*NC+1):(3*NC)] #個体-花粉親行列Individual Father Matrix: IFM
    MMM = ICMP[(NC+1):(2*NC),(NC+1):(2*NC)] #母親-母親行列Mother Mother Matrix: MMM
    MFM = ICMP[(NC+1):(2*NC),(2*NC+1):(3*NC)] #母親-花粉親行列Mother Father Matrix: MFM
    FFM = ICMP[(2*NC+1):(3*NC),(2*NC+1):(3*NC)] #花粉親-花粉親行列Father Father Matrix: FFM
    MMM <- MMM+diag(NC)#母親間行列の対角要素を0から1に変換しておく
    FFM <- FFM+diag(NC)#父親間行列の対角要素を0から1に変換しておく
    IIM = ifelse(IIM == 0, 1, 0)#血縁の強さに応じた血縁度を付与する，同クローンは1
    IMM = ifelse(IMM == 0, 0.5, 0)
    IFM = ifelse(IFM == 0, 0.5, 0)
    MMM = ifelse(MMM == 0, 0.25, 0)
    MFM = ifelse(MFM == 0, 0.25, 0)
    FFM = ifelse(FFM == 0, 0.25, 0)
    ICM = IIM+IMM+IFM+MMM+MFM+FFM #植栽木間，総当たりの近交行列
    return(ICM)
}


