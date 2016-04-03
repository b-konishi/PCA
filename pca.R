# 正規化・主成分分析を行うプログラム
# 2016/4/2
frame()

labels <- c('Before Normalization', 'After Normalization', 'After PCA')
cols <- c('red','blue','black')

## Input from Direct
#x1 <- c(3,2)
#x2 <- c(3,4)
#x3 <- c(5,4)
#x4 <- c(5,6)
#X <- cbind(x1, x2, x3, x4)

## Input from Data-File
X <- matrix(scan('./data.txt', sep=','), 2)

# 正規化前にプロットする
maxX <- max(abs(X[1,]))
maxY <- max(abs(X[2,]))
maximum <- if(maxX > maxY) maxX else maxY
plot(X[1,],X[2,],xlim=c(-maximum, maximum),ylim=c(-maximum, maximum),pch=16,col=cols[1],cex=2)
cat('X\n')
print(X)

# 正規化(平均0, 分散1)
cat('normalMatrix\n')
print(normalMatrix(X))
Y <- normalMatrix(deviation(X)) %*% deviation(X)
points(Y[1,], Y[2,], pch=16, col=cols[2], cex=2)
cat('Y\n')
print(Y)

# 分散共分散行列
cat('Variance Covariance Matrix\n')
vcm = varCovMatrix(Y)
print(vcm)

# 相関係数を求める
cat('\n################\n')
cat('Correlation[%]\n')
correlation <- vcm[1,2]^2 / (vcm[1,1]*vcm[2,2]) * 100
print(correlation)
cat('################\n\n')

# 第一主成分の固有ベクトルを取り出す
# 正規化されていれば、固有値(λ=1±a)(a:共分散), 固有ベクトル(1,±1)
vec <- eigen(vcm)[[2]][,1]
if(sign(vec[1])==-1 && sign(vec[2])==-1) vec = -vec
tilt <- if(vec[1] != 0) vec[2]/vec[1] else 0
invtilt <- 1/tilt
abline(0, tilt)
cat('eigen\n')
print(eigen(vcm))

# 主成分分析を行い、一次元にする
Z <- t(vec) %*% Y
cat('Z\n')
print(Z)
x <- (1/sqrt(1+tilt^2))*Z
y <- (1/sqrt(1+invtilt^2))*Z
if(sign(vec[1]*vec[2])==-1) if(sign(vec[1])==1) y = -y else x = -x
cat('x\n')
print(x)
cat('y\n')
print(y)
points(x, y, pch=16, col=cols[3], cex=2)

legend('topleft', legend=labels, col=cols, pch=16)


