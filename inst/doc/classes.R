###################################################
### chunk number 1: 
###################################################
owidth <- getOption("width")
options("width"=80)
ow <- getOption("warn")
options("warn"=-1)
.PngNo <- 0


###################################################
### chunk number 2: afig
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)


###################################################
### chunk number 3: zfig
###################################################
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 4: zfigkasc
###################################################
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=12cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 5: 
###################################################
library(adehabitat)


###################################################
### chunk number 6: rand
###################################################
mat <- matrix(rnorm(10000), 100, 100)
asc <- as.asc(mat)
image(asc)
box()


###################################################
### chunk number 7: 
###################################################
mat <- matrix(rnorm(10000), 100, 100)
asc <- as.asc(mat)
image(asc)
box()


###################################################
### chunk number 8: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
mat <- matrix(rnorm(10000), 100, 100)
asc <- as.asc(mat)
image(asc)
box()
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 9: 
###################################################
(path.to.file <- paste(system.file(package = "adehabitat"), 
                       "ascfiles/elevation.asc", sep = "/"))


###################################################
### chunk number 10: elev
###################################################
el <- import.asc(path.to.file)
image(el, main = "Elevation")


###################################################
### chunk number 11: 
###################################################
el <- import.asc(path.to.file)
image(el, main = "Elevation")


###################################################
### chunk number 12: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
el <- import.asc(path.to.file)
image(el, main = "Elevation")
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 13: 
###################################################
(path.to.map <- paste( system.file(package = "adehabitat"), 
                      "ascfiles/aspect.asc", sep = "/"))
asp <- import.asc(path.to.map, type = "factor")
levels(asp)


###################################################
### chunk number 14: 
###################################################
asp <- import.asc(path.to.map, type = "factor",
                  lev = c("North", "East", "West", "South"))
levels(asp)


###################################################
### chunk number 15: 
###################################################
(path.to.table <- paste(system.file(package = "adehabitat"), 
                        "ascfiles/aspect.txt", sep = "/"))


###################################################
### chunk number 16: 
###################################################
asp <- import.asc(path.to.map, type = "factor", lev = 
                  path.to.table, levnb = 1, labnb = 3)
levels(asp)
co <- colasc(asp, North = "blue", East = "yellow", 
             West = "orange", South = "red")


###################################################
### chunk number 17: asp
###################################################
image(asp, clfac = co)
legend(696662, 3166028, legend = levels(asp), fill = co)


###################################################
### chunk number 18: 
###################################################
image(asp, clfac = co)
legend(696662, 3166028, legend = levels(asp), fill = co)


###################################################
### chunk number 19: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(asp, clfac = co)
legend(696662, 3166028, legend = levels(asp), fill = co)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 20: kasc
###################################################
data(puechabon)
kasc <- puechabon$kasc
image(kasc)


###################################################
### chunk number 21: 
###################################################
data(puechabon)
kasc <- puechabon$kasc
image(kasc)


###################################################
### chunk number 22: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
data(puechabon)
kasc <- puechabon$kasc
image(kasc)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=12cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 23: 
###################################################
(obj <- as.kasc(list(Elevation = el, Aspect = asp)))


###################################################
### chunk number 24: 
###################################################
data(puechabon)
puechabon$kasc
puechabon$locs[1:4,]


###################################################
### chunk number 25: prespuech
###################################################
el <- getkasc(puechabon$kasc, "Elevation")
opar <- par(mfrow = c(2,2), mar=c(0,0,4,0))
for (i in levels(puechabon$locs$Name)) {
  image(el, 
        main = paste("Wild boar named", i),
        axes=FALSE)
  points(puechabon$locs[puechabon$locs$Name==i,c("X","Y")], pch=16)
}
par(opar)


###################################################
### chunk number 26: 
###################################################
el <- getkasc(puechabon$kasc, "Elevation")
opar <- par(mfrow = c(2,2), mar=c(0,0,4,0))
for (i in levels(puechabon$locs$Name)) {
  image(el, 
        main = paste("Wild boar named", i),
        axes=FALSE)
  points(puechabon$locs[puechabon$locs$Name==i,c("X","Y")], pch=16)
}
par(opar)


###################################################
### chunk number 27: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
el <- getkasc(puechabon$kasc, "Elevation")
opar <- par(mfrow = c(2,2), mar=c(0,0,4,0))
for (i in levels(puechabon$locs$Name)) {
  image(el, 
        main = paste("Wild boar named", i),
        axes=FALSE)
  points(puechabon$locs[puechabon$locs$Name==i,c("X","Y")], pch=16)
}
par(opar)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=12cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 28: 
###################################################
data(chamois)
chamois$map
chamois$locs[1:4,]


###################################################
### chunk number 29: preschart
###################################################
sl <- getkasc(chamois$map, "Slope")
image(sl, main = "Distribution of chamois occurrences in the Chartreuse mountain")
points(chamois$locs, pch=16)


###################################################
### chunk number 30: 
###################################################
sl <- getkasc(chamois$map, "Slope")
image(sl, main = "Distribution of chamois occurrences in the Chartreuse mountain")
points(chamois$locs, pch=16)


###################################################
### chunk number 31: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
sl <- getkasc(chamois$map, "Slope")
image(sl, main = "Distribution of chamois occurrences in the Chartreuse mountain")
points(chamois$locs, pch=16)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 32: 
###################################################
kasc <- puechabon$kasc
(el <- getkasc(kasc, "Elevation"))


###################################################
### chunk number 33: 
###################################################
er8 <- morphology(el, operation="erode", nt=8)
di8 <- morphology(el, operation="dilate", nt=8)


###################################################
### chunk number 34: morpho
###################################################
image(di8, col="black")
image(el, col="gray", add=TRUE)
image(er8, col="white", add=TRUE)

arrows(703530, 3165169, 703530, (3165169-800), code = 3, lwd = 2, length = 0.1)
text(704156, 3164775, "800 m")
arrows(704295, 3159355, 706588, 3157294, col="red", lwd = 2, code = 1)
text(706240, 3156738, "Boundary of the study area")
legend(696000, 3165841, c("Buffer area inside the boundary",
"Buffer area outside the boundary"), fill = c("gray", "black"), cex = 0.7)


###################################################
### chunk number 35: 
###################################################
image(di8, col="black")
image(el, col="gray", add=TRUE)
image(er8, col="white", add=TRUE)

arrows(703530, 3165169, 703530, (3165169-800), code = 3, lwd = 2, length = 0.1)
text(704156, 3164775, "800 m")
arrows(704295, 3159355, 706588, 3157294, col="red", lwd = 2, code = 1)
text(706240, 3156738, "Boundary of the study area")
legend(696000, 3165841, c("Buffer area inside the boundary",
"Buffer area outside the boundary"), fill = c("gray", "black"), cex = 0.7)


###################################################
### chunk number 36: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(di8, col="black")
image(el, col="gray", add=TRUE)
image(er8, col="white", add=TRUE)

arrows(703530, 3165169, 703530, (3165169-800), code = 3, lwd = 2, length = 0.1)
text(704156, 3164775, "800 m")
arrows(704295, 3159355, 706588, 3157294, col="red", lwd = 2, code = 1)
text(706240, 3156738, "Boundary of the study area")
legend(696000, 3165841, c("Buffer area inside the boundary",
"Buffer area outside the boundary"), fill = c("gray", "black"), cex = 0.7)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 37: 
###################################################
data(puechabon)
puechabon$locs[1:4,]


###################################################
### chunk number 38: ptsel
###################################################
image(el)
points(puechabon$locs[,c("X","Y")], pch = 16)


###################################################
### chunk number 39: 
###################################################
image(el)
points(puechabon$locs[,c("X","Y")], pch = 16)


###################################################
### chunk number 40: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(el)
points(puechabon$locs[,c("X","Y")], pch = 16)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 41: buffel
###################################################
bu <- buffer(puechabon$locs[,c("X","Y")], el, 500)
image(bu)
points(puechabon$locs[,c("X","Y")], pch = 16)


###################################################
### chunk number 42: 
###################################################
bu <- buffer(puechabon$locs[,c("X","Y")], el, 500)
image(bu)
points(puechabon$locs[,c("X","Y")], pch = 16)


###################################################
### chunk number 43: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
bu <- buffer(puechabon$locs[,c("X","Y")], el, 500)
image(bu)
points(puechabon$locs[,c("X","Y")], pch = 16)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 44: 
###################################################
bubis <- bu * el
mean(as.vector(bubis), na.rm = TRUE)
sd(as.vector(bubis), na.rm = TRUE)


###################################################
### chunk number 45: bufbis
###################################################
image(bubis)


###################################################
### chunk number 46: 
###################################################
image(bubis)


###################################################
### chunk number 47: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(bubis)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 48: 
###################################################
vec <- join.asc(puechabon$locs[,c("X", "Y")], el)
length(vec)
nrow(puechabon$locs)
vec[1:10]


###################################################
### chunk number 49: 
###################################################
df <- join.kasc(puechabon$locs[,c("X", "Y")], puechabon$kasc)
nrow(df)
nrow(puechabon$locs)
df[1:10,]


###################################################
### chunk number 50: 
###################################################
(cp <- count.points(puechabon$locs[,c("X","Y")],  el))


###################################################
### chunk number 51: countpoints
###################################################
image(cp)
box()


###################################################
### chunk number 52: 
###################################################
image(cp)
box()


###################################################
### chunk number 53: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(cp)
box()
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 54: 
###################################################
(cp <- count.points.id(puechabon$locs[,c("X","Y")],  puechabon$locs$Name, el))


###################################################
### chunk number 55: cpid
###################################################
image(cp)


###################################################
### chunk number 56: 
###################################################
image(cp)


###################################################
### chunk number 57: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(cp)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 58: ascgen
###################################################
hihi <- ascgen(xy = puechabon$locs[,c("X","Y")], cellsize = 500)
image(hihi)
box()


###################################################
### chunk number 59: 
###################################################
hihi <- ascgen(xy = puechabon$locs[,c("X","Y")], cellsize = 500)
image(hihi)
box()


###################################################
### chunk number 60: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
hihi <- ascgen(xy = puechabon$locs[,c("X","Y")], cellsize = 500)
image(hihi)
box()
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 61: cpidvg
###################################################
tmpbis <- count.points.id(xy = puechabon$locs[,c("X","Y")], id = puechabon$locs$Name, hihi)
image(tmpbis)


###################################################
### chunk number 62: 
###################################################
tmpbis <- count.points.id(xy = puechabon$locs[,c("X","Y")], id = puechabon$locs$Name, hihi)
image(tmpbis)


###################################################
### chunk number 63: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
tmpbis <- count.points.id(xy = puechabon$locs[,c("X","Y")], id = puechabon$locs$Name, hihi)
image(tmpbis)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 64: 
###################################################
el <- getkasc(puechabon$kasc, "Elevation")
elcat <- el < 200
class(elcat)
names(attributes(elcat))


###################################################
### chunk number 65: 
###################################################
(elcat<-getascattr(el, elcat, type = "factor", lev = c("> 200 m", "< 200 m")))


###################################################
### chunk number 66: elcat
###################################################
image(elcat)
legend(698000, 3165000, levels(elcat), fill=rainbow(2))


###################################################
### chunk number 67: 
###################################################
image(elcat)
legend(698000, 3165000, levels(elcat), fill=rainbow(2))


###################################################
### chunk number 68: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(elcat)
legend(698000, 3165000, levels(elcat), fill=rainbow(2))
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 69: managna
###################################################
kasc <- puechabon$kasc
el <- getkasc(kasc, "Elevation")
sl <- getkasc(kasc, "Slope")
el[el < 200] <- NA
tmp <- as.kasc(list(Elevation = el, Slope = sl))
image(tmp)


###################################################
### chunk number 70: 
###################################################
kasc <- puechabon$kasc
el <- getkasc(kasc, "Elevation")
sl <- getkasc(kasc, "Slope")
el[el < 200] <- NA
tmp <- as.kasc(list(Elevation = el, Slope = sl))
image(tmp)


###################################################
### chunk number 71: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
kasc <- puechabon$kasc
el <- getkasc(kasc, "Elevation")
sl <- getkasc(kasc, "Slope")
el[el < 200] <- NA
tmp <- as.kasc(list(Elevation = el, Slope = sl))
image(tmp)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 72: manna
###################################################
tmp <- managNAkasc(tmp)
image(tmp)


###################################################
### chunk number 73: 
###################################################
tmp <- managNAkasc(tmp)
image(tmp)


###################################################
### chunk number 74: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
tmp <- managNAkasc(tmp)
image(tmp)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 75: 
###################################################
data(puechabon)
kasc <- puechabon$kasc
toto <- kasc[1:10,]
class(toto) <- "data.frame"
toto


###################################################
### chunk number 76: 
###################################################
huhu <- kasc2df(kasc)
names(huhu)
huhu$index[1:4]
huhu$tab[1:4,]


###################################################
### chunk number 77: 
###################################################
huhu$tab$Aspect <- NULL
(pc <- dudi.pca(huhu$tab, scannf =FALSE, nf=2))


###################################################
### chunk number 78: df2kasc
###################################################
map <- df2kasc(pc$li, huhu$index, kasc)
image(map)


###################################################
### chunk number 79: 
###################################################
map <- df2kasc(pc$li, huhu$index, kasc)
image(map)


###################################################
### chunk number 80: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
map <- df2kasc(pc$li, huhu$index, kasc)
image(map)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=12cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 81: 
###################################################
(kasc <- chamois$map)
(si1 <- object.size(kasc))


###################################################
### chunk number 82: donncham
###################################################
image(kasc)


###################################################
### chunk number 83: 
###################################################
image(kasc)


###################################################
### chunk number 84: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(kasc)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=12cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 85: 
###################################################
(m <- lowres(kasc, np = 4))


###################################################
### chunk number 86: lowres
###################################################
image(m)


###################################################
### chunk number 87: 
###################################################
image(m)


###################################################
### chunk number 88: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(m)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=12cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 89: 
###################################################
(si2 <- object.size(m))
(si1 - si2)/si1


###################################################
### chunk number 90: subset
###################################################
data(chamois)
slope <- getkasc(chamois$map, "Slope")
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,1,1,1,1,1,1,1,2), ncol = 3, byrow = TRUE))
par(mar = c(0,0,0,0))
image(slope, axes=FALSE)
box()

x <- c(863603.8, 867286.5)
y <- c(2042689, 2045797)
polygon(x = c(x[1], x[2], x[2], x[1]),
        y = c(y[1], y[1], y[2], y[2]), lwd=2)

sl2 <- subsetmap(slope, xlim = x, ylim = y)
par(mar = c(0,0,2,0))
image(sl2, axes = FALSE, main = "Reduced map")
box()
par(def.par)


###################################################
### chunk number 91: 
###################################################
data(chamois)
slope <- getkasc(chamois$map, "Slope")
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,1,1,1,1,1,1,1,2), ncol = 3, byrow = TRUE))
par(mar = c(0,0,0,0))
image(slope, axes=FALSE)
box()

x <- c(863603.8, 867286.5)
y <- c(2042689, 2045797)
polygon(x = c(x[1], x[2], x[2], x[1]),
        y = c(y[1], y[1], y[2], y[2]), lwd=2)

sl2 <- subsetmap(slope, xlim = x, ylim = y)
par(mar = c(0,0,2,0))
image(sl2, axes = FALSE, main = "Reduced map")
box()
par(def.par)


###################################################
### chunk number 92: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
data(chamois)
slope <- getkasc(chamois$map, "Slope")
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,1,1,1,1,1,1,1,2), ncol = 3, byrow = TRUE))
par(mar = c(0,0,0,0))
image(slope, axes=FALSE)
box()

x <- c(863603.8, 867286.5)
y <- c(2042689, 2045797)
polygon(x = c(x[1], x[2], x[2], x[1]),
        y = c(y[1], y[1], y[2], y[2]), lwd=2)

sl2 <- subsetmap(slope, xlim = x, ylim = y)
par(mar = c(0,0,2,0))
image(sl2, axes = FALSE, main = "Reduced map")
box()
par(def.par)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 93: 
###################################################
data(elec88)
ar <- elec88$area
ar[1:5,]


###################################################
### chunk number 94: area
###################################################
ar <- as.area(ar)
area.plot(ar)


###################################################
### chunk number 95: 
###################################################
ar <- as.area(ar)
area.plot(ar)


###################################################
### chunk number 96: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
ar <- as.area(ar)
area.plot(ar)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 97: 
###################################################
data(puechabon)
lo <- puechabon$locs
cp <- mcp(lo[,c("X", "Y")], lo[,"Name"])
class(cp)


###################################################
### chunk number 98: convpol
###################################################
opar <- par(mar=c(0,0,0,0))
area.plot(cp)
points(puechabon$locs[,c("X", "Y")], pch=16, col = as.numeric(puechabon$locs$Name))
box()
par(opar)


###################################################
### chunk number 99: 
###################################################
opar <- par(mar=c(0,0,0,0))
area.plot(cp)
points(puechabon$locs[,c("X", "Y")], pch=16, col = as.numeric(puechabon$locs$Name))
box()
par(opar)


###################################################
### chunk number 100: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
opar <- par(mar=c(0,0,0,0))
area.plot(cp)
points(puechabon$locs[,c("X", "Y")], pch=16, col = as.numeric(puechabon$locs$Name))
box()
par(opar)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 101: 
###################################################
el <- getkasc(puechabon$kasc, "Elevation")
cont.el <- getcontour(el)
class(cont.el)
nlevels(cont.el[,1])


###################################################
### chunk number 102: getcontour
###################################################
image(el)
polygon(cont.el[,2:3], lwd = 3)


###################################################
### chunk number 103: 
###################################################
image(el)
polygon(cont.el[,2:3], lwd = 3)


###################################################
### chunk number 104: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
image(el)
polygon(cont.el[,2:3], lwd = 3)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 105: 
###################################################
lo <- puechabon$locs
kasc <- puechabon$kasc
cp <- mcp(lo[,c("X", "Y")], lo[,"Name"])


###################################################
### chunk number 106: 
###################################################
(rast <- hr.rast(cp, kasc))


###################################################
### chunk number 107: mcprast
###################################################
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,1,2,4,3,5),2,3))
par(mar=c(0,0,4,0))
area.plot(cp)
points(puechabon$locs[,c("X", "Y")], pch=16, col = as.numeric(puechabon$locs$Name))
box()
for (i in names(rast)) {
image(getkasc(rast,i), main = paste("Wild boar named", i), axes=FALSE)
polygon(cont.el[,2:3])
box()
}
par(def.par)


###################################################
### chunk number 108: 
###################################################
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,1,2,4,3,5),2,3))
par(mar=c(0,0,4,0))
area.plot(cp)
points(puechabon$locs[,c("X", "Y")], pch=16, col = as.numeric(puechabon$locs$Name))
box()
for (i in names(rast)) {
image(getkasc(rast,i), main = paste("Wild boar named", i), axes=FALSE)
polygon(cont.el[,2:3])
box()
}
par(def.par)


###################################################
### chunk number 109: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,1,2,4,3,5),2,3))
par(mar=c(0,0,4,0))
area.plot(cp)
points(puechabon$locs[,c("X", "Y")], pch=16, col = as.numeric(puechabon$locs$Name))
box()
for (i in names(rast)) {
image(getkasc(rast,i), main = paste("Wild boar named", i), axes=FALSE)
polygon(cont.el[,2:3])
box()
}
par(def.par)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=12cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 110: polmask
###################################################
el <- getkasc(puechabon$kasc, "Elevation")
pol <- data.frame(x = c(700658, 699222, 698342, 698643, 700427, 701029),
                  y = c(3160768, 3160676, 3159402, 3158336, 3158869, 3159657))
image(el)
polygon(pol, lwd=2)


###################################################
### chunk number 111: 
###################################################
el <- getkasc(puechabon$kasc, "Elevation")
pol <- data.frame(x = c(700658, 699222, 698342, 698643, 700427, 701029),
                  y = c(3160768, 3160676, 3159402, 3158336, 3158869, 3159657))
image(el)
polygon(pol, lwd=2)


###################################################
### chunk number 112: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
el <- getkasc(puechabon$kasc, "Elevation")
pol <- data.frame(x = c(700658, 699222, 698342, 698643, 700427, 701029),
                  y = c(3160768, 3160676, 3159402, 3158336, 3158869, 3159657))
image(el)
polygon(pol, lwd=2)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 113: mask
###################################################
pr <- mcp.rast(pol, el)
masked.kasc <- setmask(puechabon$kasc, pr)
image(masked.kasc, xlim = c(696999, 702373), 
      ylim = c(3156784, 3162297))


###################################################
### chunk number 114: 
###################################################
pr <- mcp.rast(pol, el)
masked.kasc <- setmask(puechabon$kasc, pr)
image(masked.kasc, xlim = c(696999, 702373), 
      ylim = c(3156784, 3162297))


###################################################
### chunk number 115: 
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".png", sep="")
png(file=file, width = 700, height = 700, pointsize = 12, bg = "white")
opar <- par(no.readonly = TRUE)
pr <- mcp.rast(pol, el)
masked.kasc <- setmask(puechabon$kasc, pr)
image(masked.kasc, xlim = c(696999, 702373), 
      ylim = c(3156784, 3162297))
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[height=7cm,keepaspectratio]{", file, "}\n\n", sep="")


###################################################
### chunk number 116: 
###################################################
def.pol <- function(x) {
  toto<-locator(1)
  for (i in 2:x) {
    tutu<-locator(1)
    toto$x<-c(toto$x, tutu$x)
    toto$y<-c(toto$y, tutu$y)
    lines(toto$x, toto$y)
  }
  polygon(toto)
  return(toto)
}


