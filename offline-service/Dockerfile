FROM openjdk:21
ADD target/*.jar offline-service
ENTRYPOINT ["java", "-jar","offline-service"]
EXPOSE 80