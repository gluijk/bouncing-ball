# Bouncing ball LLM torture test
# www.overfitting.net
# https://www.overfitting.net/


library(png)

NewBitmap = function(dimx, dimy, val=0) {
    # Crea bitmap de dimensiones dimx y dimy
    return(array(val,c(dimx,dimy)))
}

# Por Carlos Gil Bellosta
indices.drawline = function(x0, y0, x1, y1) {
    x0=round(x0)
    x1=round(x1)
    y0=round(y0)
    y1=round(y1)
    
    if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
    if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
        m = (y1 - y0) / (x1 - x0)
        cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
    } else indices.drawline(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
    # Llamada traspuesta recursiva y traspuesta
}

DrawLine = function(img, x0, y0, x1, y1, inc=TRUE, val=1) {
    # Dibuja recta desde (x0,y0)-(x1,y1)
    # Por defecto método no destructivo y con valor=1
    indices=indices.drawline(x0, y0, x1, y1)
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawPoint = function(img, x0, y0, inc=TRUE, val=1) {
    # Dibuja punto en (x0,y0)
    # Por defecto método no destructivo y con valor=1
    img=DrawLine(img, x0, y0, x0, y0, inc, val)
    
    return(img)
}

DrawEllip = function(img, x0, y0, a, b, inc=TRUE, val=1, fill=FALSE, thick=1) {
    # Dibuja elipse de centro (x0,y0) y radios a y b
    # Por defecto método no destructivo, con valor=1 y sin relleno
    # Puede elegirse el grosor si no se rellena
    # Aquí no redondeamos para tener más precisión en la división
    if (fill) {
        indices=which( ((row(img)-x0)/a)^2 + ((col(img)-y0)/b)^2 < 1 )
    } else {
        indices=which( ((row(img)-x0)/(a+thick/2))^2 + ((col(img)-y0)/(b+thick/2))^2 <  1 &
                       ((row(img)-x0)/(a-thick/2))^2 + ((col(img)-y0)/(b-thick/2))^2 >= 1 )
    }
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawCircle = function(img, x0, y0, r, inc=TRUE, val=1, fill=FALSE, thick=1) {
    # Dibuja círculo de centro (x0,y0) y radio r
    # Por defecto método no destructivo, con valor=1 y sin relleno
    # Puede elegirse el grosor si no se rellena
    img=DrawEllip(img, x0, y0, r, r, inc, val, fill, thick)
    
    return(img)
}

LoadBitmap = function(name, chan=2) {
    # Lee bitmap en formato PNG
    # Si no es monocromo se carga el canal chan (por defecto G)
    require(png)
    img=readPNG(name)
    if (length(dim(img))>2) img=img[,,chan]
    
    return(t(img[nrow(img):1,]))
}

SaveBitmap = function(img, name, trunc=TRUE, gamma=1) {
    # Guarda bitmap en formato PNG
    # Solo si trunc=FALSE y la imagen excede de 1 se reescala a 1
    require(png)
    img[img<0]=0
    if (trunc) img[img>1]=1
    if (tolower(substr(name, nchar(name)-3, nchar(name))) != ".png") name=paste0(name,".png")
    writePNG(t(img[,ncol(img):1] / max(max(img),1))^(1/gamma), name)
}


# Functions created by ChatGPT:

update_position <- function(xp, yp, phi, D) {
    # ChatGPT prompt:
    # "write a R function that updates the position of a particle
    # located at (xp,yp) moving in the direction phi, travelling
    # a distance of D"
    
    # Calculate the new position
    x_new <- xp + D * cos(phi)
    y_new <- yp + D * sin(phi)
    
    return(c(x_new, y_new))
}

rotate_point <- function(xp, yp, theta) {
    # ChatGPT prompt:
    # "write a R function that calculates the rotation of a point (xp,yp)
    # around the origin (0,0) by an amount of theta radians"
    
    # Calculate the rotated coordinates
    x_rot <- xp * cos(theta) - yp * sin(theta)
    y_rot <- xp * sin(theta) + yp * cos(theta)
    
    return(c(x_rot, y_rot))
}

distance_to_line <- function(xp, yp, x0, y0, x1, y1) {
    # ChatGPT prompt:
    # "write a R function that calculates the distance between
    # a point (xp,yp) and the line defined by points (x0,y0) and (x1,y1)"
    
    # Calculate the coefficients of the line equation: Ax + By + C = 0
    A <- y1 - y0
    B <- x0 - x1
    C <- x1 * y0 - x0 * y1
    
    # Calculate the perpendicular distance using the formula
    distance <- abs(A * xp + B * yp + C) / sqrt(A^2 + B^2)
    
    return(distance)
}

specular_bounce_WRONG <- function(phi, x0, y0, x1, y1) {
    # ChatGPT prompt
    # "write a R function that calculates the new moving direction
    # of a particle currently moving in the direction defined by phi radians,
    # after a collision with a line defined by points (x0,y0) and (x1,y1)
    # assuming a specular bounce on the line"
    
    # Calculate the angle of the line (normal vector's perpendicular)
    theta_line <- atan2(y1 - y0, x1 - x0)  # Line direction angle
    
    # Calculate the normal to the line (perpendicular angle)
    theta_normal <- theta_line + pi / 2  # Normal is perpendicular to the line
    
    # Compute the reflection angle (specular bounce)
    phi_new <- 2 * theta_normal - phi  # adding  + pi it works
    
    # Normalize the angle to be between 0 and 2*pi
    phi_new <- (phi_new + 2 * pi) %% (2 * pi)
    
    return(phi_new)
}

specular_bounce <- function(phi, x0, y0, x1, y1) {
    # ChatGPT prompt
    # "write a R function that calculates the new moving direction
    # of a particle currently moving in the direction defined by phi radians,
    # after a collision with a line defined by points (x0,y0) and (x1,y1)
    # assuming a specular bounce on the line"
    
    # Calculate the angle of the line with respect to the x-axis
    theta_line <- atan2(y1 - y0, x1 - x0)  # Line direction angle
    
    # Compute the reflected direction after the bounce
    phi_new <- 2 * theta_line - phi
    
    # Normalize the angle to be in the range [0, 2*pi)
    phi_new <- (phi_new + 2 * pi) %% (2 * pi)
    
    return(phi_new)
}


####################################

# Animation parameters:
DIMY=512
DIMX=512
OFFY=DIMY/2
OFFX=DIMX/2

# Ball definition:
R=10  # radius of ball
xp=0  # starting position of ball
yp=0
phi=pi/4  # starting direction of ball
D=3  # ball position increase after each iteration
COLLISIONDELAY=10

# Square definition:
L=200  # length of square sides
xsquareini=c(L/2, L/2,-L/2,-L/2)  # starting square corners
ysquareini=c(L/2,-L/2,-L/2, L/2)
xsquare=xsquareini
ysquare=ysquareini
dtheta=2*pi/360/8  # square theta increase after each iteration


# Add 'PING!'
library(tuneR)

boing=readWave("boing.wav")
play(boing)
boing
LENAUDIO=length(boing@left)
fps=24
fs=boing@samp.rate
bits=boing@bit
TOTALSAMPLES=fs*NFRAMES/fps
sonido=array(0, TOTALSAMPLES)


# Build frames
NFRAMES=360*8
collisioncount=0
for (frame in 0:(NFRAMES-1)) {
    # New empty frame
    img=NewBitmap(DIMY, DIMY)
    
    # Draw ball
    img=DrawCircle(img, xp+OFFX, yp+OFFY, R, fill=TRUE, val=0.25)
    
    # Draw rectangle
    for (i in 1:4) {
        img=DrawLine(img, xsquare[i]+OFFX, ysquare[i]+OFFY,
            xsquare[ifelse(i==4,1,i+1)]+OFFX, ysquare[ifelse(i==4,1,i+1)]+OFFY,
            inc=FALSE)
    }

    # Check for collision and eventually update ball moving direction
    # and add PING! to audio track
    collisioncount=collisioncount+1
    for (i in 1:4) {
        dist=distance_to_line(xp, yp, xsquare[i], ysquare[i],
                              xsquare[ifelse(i==4,1,i+1)], ysquare[ifelse(i==4,1,i+1)])
        if (dist <= R & collisioncount>=COLLISIONDELAY) {
            phi=specular_bounce(phi, xsquare[i], ysquare[i],
                                xsquare[ifelse(i==4,1,i+1)], ysquare[ifelse(i==4,1,i+1)])
            INIAUDIO=round(TOTALSAMPLES-1)/NFRAMES*frame+1
            sonido[INIAUDIO:(INIAUDIO+LENAUDIO-1)]=
                sonido[INIAUDIO:(INIAUDIO+LENAUDIO-1)]+boing@left
            collisioncount=0
        }
    }
    
    # Update ball position
    updateball=update_position(xp, yp, phi, D)
    xp=updateball[1]
    yp=updateball[2]
    
    # Update rectangle rotation
    for (i in 1:4) {
        updatesquare=rotate_point(xsquareini[i], ysquareini[i], dtheta*(frame+1))
        xsquare[i]=updatesquare[1]
        ysquare[i]=updatesquare[2]
    }   

    # Save frame
    name=paste0("bouncingball", ifelse(frame<10, "000",
                                ifelse(frame<100, "00",
                                ifelse(frame<1000, "0", ""))), frame, ".png")
    print(paste0(frame+1, "/", NFRAMES, ": Writing '", name, "'..."))
    SaveBitmap(img, name)
}

# Save soundtrack
max(abs(boing@left))  # 26.363
sonido=sonido[1:TOTALSAMPLES]
max(sonido)  # 24.093
boing@left=sonido
writeWave(boing, filename="bouncingballaudiotrack.wav")



# SALIDA VÍDEO

# MP4 Video (MPEG-4 AVC/H.264):
# ffmpeg -loop 1 -framerate 24 -i bouncingball%04d.png -i /
# bouncingballaudiotrack.wav -t 120 -c:v libx264 -crf 23 -pix_fmt yuv420p /
# bouncingball.mp4

# GIF animado:
# magick -delay 3 -loop 0 bouncingball*.png bouncingball.gif


