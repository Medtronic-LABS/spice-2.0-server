FROM openjdk:21
ADD target/*.jar fhir-mapper
ENTRYPOINT ["java", "-jar","fhir-mapper"]
EXPOSE 80