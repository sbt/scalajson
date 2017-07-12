jenv shell 1.6
sbt
> clean
> ++2.10.6
> scalaJsonJVM/publishSigned
> ++2.11.11
> publishSigned
> exit

jenv shell 1.8
sbt
> clean
> ++2.12.2
> publishSigned
> exit
