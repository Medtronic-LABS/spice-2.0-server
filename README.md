# Spice Opensource 2.0

This is the backend of the Spice app, which provides healthcare support to patients across a population.

## Prerequisites

- Git
- Java 21.x
- Apache Maven 3.8.x
- Docker 20.10.xx
- Docker-compose 1.29.x
- Postgres 15.X
- [HAPI FHIR JPA Server]

## Installation

To bring the Spice backend up and running, there are few prerequisite that has to be done.
You can either follow the commands or access the official documentation by clicking the hyperlink.

- To install git in `ubuntu` run the following command or click [Git Official site].

```sh
sudo apt install git
```

- To install git in `windows` visit [Git Official site].


- To install java in `ubuntu` run the following command or click [Java Official site].

```sh
sudo apt install openjdk-21 -jdk
```

- To install java in `windows` visit [Java Official site].


- To download and install `maven` in `ubuntu` click [Maven Official site - Download]
  and [Maven Official site - Install].


- After installing the maven zip file open the download folder and extract it.


- To set maven home path in Ubuntu run the following command.

```sh
nano ~/.bashrc
```

- Paste the following lines in .bashrc file.

```Environment variable
MAVEN_HOME='/home/ubuntu/Downloads/apache-maven-3.8.8'
PATH="$MAVEN_HOME/bin:$PATH"
export PATH
```

- Then save the .bashrc file.
- To apply the changes in the .bashrc file run the following command.

```sh
. ~/.bashrc
```

- Note: Once you have executed the script mentioned above, please restart the machine.
- To verify the maven installation, run the following command and check the version

```sh
mvn -v
```

- To download `maven` in `windows` visit [Maven Official site - Download].
- To install `maven` in `windows` visit [Maven Official site - Install].

- To install docker follow steps in the [Docker Official Docs]
- To install docker-compose in `ubuntu` follow steps in the [Docker Compose Official Docs]

## Setup

- Clone the spice-server repository.

```sh
git clone https://github.com/Medtronic-LABS/spice-2.0-server.git
```

## Configuration

To run the application, you should pass the necessary configuration via environment properties.
To achieve this, create a ***.env*** file and pass your own values for the following properties.

>**Note:**
Please paste the ***.env*** file inside the specified directory.

`/spice-2.0-server/`

***.env*** **file**

```properties
PROJECT_PATH=/home/ubuntu/spice-2.0-server
DATABASE_URL=jdbc:postgresql://{{ip}}:5432/spice_open_source?serverTimezone=UTC
DATABASE_USERNAME={{username}}
DATABASE_PASSWORD={{password}}
DATABASE_NAME=spice_open_source
DB_MAX_POOL_SIZE=10
REDIS_PASSWORD={{password}}
REDIS_HOST={{ip}}
REDIS_PORT=6379

CFR_SERVICE=http://cfrservice:8888
USER_SERVICE=http://userservice:8085
ADMIN_SERVICE=http://adminservice:8086
SPICE_SERVICE=http://spiceservice:8087
AUTH_SERVICE=http://authservice:8089
FHIR_SERVICE=http://fhirmapper:8092
CQL_SERVICE=http://cqlservice:8094
NOTIFICATION_SERVICE=http://notificationservice:8084
SHORTEN_URL_DOMAIN=http://{{ip}}/shorturl-service
HAPI_SERVER_URL=http://{{ip}}:8080/fhir/

MAIL_SEND=false
EMAIL_APP_URL=http://spicetest.com/reset-password/
MAIL_FROM={{mail}}
MAIL_HOST=smtp.sendgrid.net
MAIL_PORT=587
MAIL_PASSWORD={{password}}
MAIL_USER={{apiKey}}
SHEDLOCK_EMAIL_START=PT30S
SHEDLOCK_EMAIL_STOP=PT02M

ENABLE_SMS_NOTIFICATION=false
ENABLE_SMS_NOTIFICATION_SNS=false
ENABLE_SMS_NOTIFICATION_TWILIO=false
TWILIO_ACCOUNT_SID={{sid}}
TWILIO_AUTH_TOKEN={{token}}
TWILIO_FROM_PHONENO={{phoneNumber}}
ENABLE_SMS_NOTIFICATION_DIGIMILES=false
DIGIMILES_SMS_DLR=1
DIGIMILES_SMS_USERNAME={{username}}
DIGIMILES_SMS_PASSWORD={{password}}
DIGIMILES_SMS_SENDER=sender
DIGIMILES_SMS_TYPE=0
DIGIMILES_SMS_URL=http://global.digimiles.in:8080/bulksms/bulksms
IS_NEXT_MEDICAL_REVIEW_JOB=false
IS_NEXT_BP_JOB=false
IS_NEXT_BG_JOB=false
ENABLE_RED_RISK_NOTIFICATION=false
SHEDLOCK_OUTBOUND_START=PT24H
SHEDLOCK_OUTBOUND_STOP=PT25H
SHEDLOCK_SMS_START=PT30S
SHEDLOCK_SMS_STOP=PT02M

SERVER_REGION=us-east-1
IS_MINIO_SERVER_ENABLE=true
MINIO_URL=http://{{ip}}:9000
MINIO_CONSOLE_URL=http://{{ip}}:9090
MINIO_ACCESS_NAME={{accessName}}
MINO_SECRET_KEY={{secretKey}}
BUCKET_NAME=spice-os
SIGNATURE_BUCKET_FOLDER=signature/
CONSENT_FORM_BUCKET_FOLDER=consent/

IS_AWS_SQS_ENABLED=false
SQS_REQUEST_QUEUE_URL=
SQS_REQUEST_QUEUE_NAME=
IS_RABBITMQ_SQS_ENABLED=true
RABBIT_MQ_HOST={{ip}}
RABBIT_MQ_PORT=5672
RABBIT_MQ_USERNAME=guest
RABBIT_MQ_PASSWORD=guest
RABBIT_MQ_EXCHANGE_NAME=SPICE_EXCHANGE
RABBIT_MQ_GROUP=SPICE_GROUP
RABBIT_PRODUCER_CHANNEL=RabbitMQProducerService-out-0

APP_VERSION=2.0.0
SMART_ANC_ENABLED=false
FORGOT_PASSWORD_TIME_LIMIT_IN_MINUTES=60
HAPI_IDENTIFIER_URL=http://mdtlabs.com/
ENVIRONMENT=dev
JOB_USER_NAME=jobuser@test.com
JOB_USER_PASSWORD=3901e08e724bb73a72137e03e2f03d54d70eaeea39f8a7e0459f15d9e80585ffd2159283b8324d72f7ba8c2aa2f0e82e5c1b685d7ef569e2ebad02d71ba4076e
SUCCESSFUL_CALL_ATTEMPTS=3
UNSUCCESSFUL_CALL_ATTEMPTS=5
FOLLOWUP_MALARIA_IN_DAYS=3
FOLLOWUP_PNEUMONIA_IN_DAYS=5
FOLLOWUP_DIARRHEA_IN_DAYS=3
FOLLOWUP_ESCALATION_LIMIT_IN_DAYS=7
FOLLOWUP_REFERRAL_IN_DAYS=2
FOLLOWUP_MUAC_IN_DAYS=7
FOLLOWUP_ANC_VISIT_IN_DAYS=2
FOLLOWUP_PNC_VISIT_IN_DAYS=2
FOLLOWUP_CHILD_VISIT_IN_DAYS=2
FOLLOWUP_CALL_ATTEMPTS=5
SCREENING_FOLLOWUP_REMAINING_DAYS=3
ASSESSMENT_FOLLOWUP_REMAINING_DAYS=5
MEDICAL_REVIEW_FOLLOWUP_REMAINING_DAYS=5
LOST_TO_FOLLOWUP_REMAINING_DAYS=90
RETRY_CALL_ATTEMPTS=5
SCREENING_FOLLOWUP_REMAINING_DAYS=3

```
>Note: The values for the environmental variables should be changed based on the chosen service.

## .env

The `.env` file is used to store environment variables for the project. These variables are used to configure the
application and contain sensitive information such as passwords, API keys, and other credentials.

Please note that the `.env` file should never be committed to version control, as it contains sensitive information that
should not be shared publicly. Instead, you should add the `.env` file to your .gitignore file to ensure that it is not
accidentally committed.

To use the application, you will need to create a `.env` file in the root directory of the project and add the necessary
environment variables. You can refer to the above file for an example of the required variables and their format.

***The values provided in the
instructions are for demonstration purposes only and will not work as-is. You will need to replace them with actual
values that are specific to your environment.***

> Note: After checking out the project, please ensure that you update the relevant properties and values in env.example, and then rename it to .env.

## .env description

`PROJECT_PATH`: This property specifies the file path to the project's backend directory in the server.

`DATABASE_URL`: This property contains the JDBC connection URL for the PostgreSQL database server with the server
timezone set to UTC, with the database name.

`DATABASE_USERNAME`: This property contains the username used to connect to the PostgreSQL database server.

`DATABASE_PASSWORD`: This property contains the password to connect to the PostgreSQL database server.

`DATABASE_NAME`: This property contains the name of the PostgreSQL database used by the project.

`DB_MAX_POOL_SIZE`: This property specifies the maximum connection pool size.

`REDIS_PASSWORD`: This property contains the password used to authenticate with the Redis server.

`REDIS_HOST`: This property contains the IP address of the Redis server.

`REDIS_PORT`: This property contains the port number used by the Redis server.

`CFR_SERVICE`: This property specifies the server address of the CFR service.

`USER_SERVICE`: This property specifies the server address of the User service.

`ADMIN_SERVICE`: This property specifies the server address of the Admin service.

`SPICE_SERVICE`: This property specifies the server address of the Spice service.

`AUTH_SERVICE`: This property specifies the server address of the Auth service.

`FHIR_SERVICE`: This property specifies the server address of the FHIR mapper service.

`CQL_SERVICE`: This property specifies the server address of the CQL service.

`NOTIFICATION_SERVICE`: This property specifies the server address of the Notification service.

`HAPI_SERVER_URL`: This property specifies the server address of the HAPI server.

`SHORTEN_URL_DOMAIN`: This property specifies the server address of the Shorten URL service.

`MAIL_SEND`: The Application uses this property to indicate whether it sends emails.

`EMAIL_APP_URL`: This property specifies the app url of the application

`MAIL_FROM`: This property specifies the verified sender's email.

`MAIL_HOST`: This property specifies the mail host.

`MAIL_PORT`: This property specifies the mail port number.

`MAIL_PASSWORD`: This property specifies the mail password.

`MAIL_USER`: This property specifies username.

`SHEDLOCK_EMAIL_START`: This property specifies the start time for the email shedlock job.

`SHEDLOCK_EMAIL_STOP`: This property specifies the stop time for the email shedlock job.

`ENABLE_SMS_NOTIFICATION`: This property indicates whether SMS notifications are enabled in the Application.

`ENABLE_SMS_NOTIFICATION_SNS`: This property indicates whether the SMS to be sent through AWS SNS (Simple Notification Service).

`ENABLE_SMS_NOTIFICATION_TWILIO`: This property indicates whether the SMS to be sent through twilio.

`TWILIO_ACCOUNT_SID`: This property specifies the twilio account sid.

`TWILIO_AUTH_TOKEN`: This property specifies the twilio auth token.

`TWILIO_FROM_PHONENO`: This property specifies the twilio phone number.

`ENABLE_SMS_NOTIFICATION_DIGIMILES`: This property indicates whether the SMS to be sent through Digimiles.

`DIGIMILES_SMS_DLR`: Specify delivery report type for SMS sent through Digimiles.

`DIGIMILES_SMS_USERNAME`: Specifies the username for Digimiles SMS authentication.

`DIGIMILES_SMS_PASSWORD`: Defines the password for Digimiles SMS authentication.

`DIGIMILES_SMS_SENDER`: Represents the sender ID for outgoing SMS messages.

`DIGIMILES_SMS_TYPE`: Specifies the type of SMS.

`DIGIMILES_SMS_URL`: The endpoint URL for sending SMS through Digimiles.

`IS_NEXT_MEDICAL_REVIEW_JOB`: This property indicates whether the next medical review job is enabled.

`IS_NEXT_BP_JOB`: This property indicates whether the next blood pressure job is enabled.

`IS_NEXT_BG_JOB`: This property indicates whether the next blood glucose job is enabled.

`ENABLE_RED_RISK_NOTIFICATION`: This property indicates whether the red risk notification is enabled.

`SHEDLOCK_OUTBOUND_START`: This property specifies the start time for the outbound shedlock job.

`SHEDLOCK_OUTBOUND_STOP`: This property specifies the stop time for the outbound shedlock job.

`SHEDLOCK_SMS_START`: This property specifies the start time for the SMS shedlock job.

`SHEDLOCK_SMS_STOP`: This property specifies the stop time for the SMS shedlock job.

`SERVER_REGION`: This property specifies the region where the Amazon S3 buckets should create.

`IS_MINIO_SERVER_ENABLE`: This property indicates whether the MinIO server is enabled.

`MINIO_URL`: The URL of the MinIO server for accessing stored objects.

`MINIO_CONSOLE_URL`: The URL for the MinIO web console.

`MINIO_ACCESS_NAME`: The access key used for authentication with MinIO.

`MINIO_SECRET_KEY`: The secret key used for authentication with MinIO.

`BUCKET_NAME`: The name of the MinIO bucket where data is stored.

`SIGNATURE_BUCKET_FOLDER`: The folder within the bucket designated for storing signatures.

`CONSENT_FORM_BUCKET_FOLDER`: The folder within the bucket designated for storing consent forms.

`IS_AWS_SQS_ENABLED`: Indicates whether AWS SQS (Simple Queue Service) is enabled.

`SQS_REQUEST_QUEUE_URL`: The URL of the AWS SQS request queue.

`SQS_REQUEST_QUEUE_NAME`: The name of the AWS SQS request queue.

`IS_RABBITMQ_SQS_ENABLED`: Indicates whether RabbitMQ is enabled as a message queue service.

`RABBIT_MQ_HOST`: The hostname or IP address of the RabbitMQ server.

`RABBIT_MQ_PORT`: The port number for connecting to RabbitMQ.

`RABBIT_MQ_USERNAME`: The username for RabbitMQ authentication.

`RABBIT_MQ_PASSWORD`: The password for RabbitMQ authentication.

`RABBIT_MQ_EXCHANGE_NAME`: The name of the RabbitMQ exchange used for message routing.

`RABBIT_MQ_GROUP`: The consumer group name for RabbitMQ messages.

`RABBIT_PRODUCER_CHANNEL`: The producer channel name for publishing messages to RabbitMQ.

`APP_VERSION`: This property specifies the app version of the spice application in the mobile device.

`SMART_ANC_ENABLED`: Indicates whether the Smart ANC (Antenatal Care) feature is enabled.

`FORGOT_PASSWORD_TIME_LIMIT_IN_MINUTES`: Defines the time limit (in minutes) for password reset requests.

`HAPI_IDENTIFIER_URL`: The base URL for the HAPI FHIR server.

`ENVIRONMENT`: Specifies the current application environment (dev/prod).

`JOB_USER_NAME`: The username for automated job execution.

`JOB_USER_PASSWORD`: The hashed password for automated job execution.

`SUCCESSFUL_CALL_ATTEMPTS`: The number of successful call attempts.

`UNSUCCESSFUL_CALL_ATTEMPTS`: The number of failed call attempts.

`FOLLOWUP_MALARIA_IN_DAYS`: The number of days for malaria follow-up.

`FOLLOWUP_PNEUMONIA_IN_DAYS`: The number of days for pneumonia follow-up.

`FOLLOWUP_DIARRHEA_IN_DAYS`: The number of days for diarrhea follow-up.

`FOLLOWUP_ESCALATION_LIMIT_IN_DAYS`: The time limit (in days) before escalating a follow-up case.

`FOLLOWUP_REFERRAL_IN_DAYS`: The number of days for referral follow-up.

`FOLLOWUP_MUAC_IN_DAYS`: The number of days for mid-upper arm circumference (MUAC) follow-up.

`FOLLOWUP_ANC_VISIT_IN_DAYS`: The number of days for antenatal care (ANC) visit follow-up.

`FOLLOWUP_PNC_VISIT_IN_DAYS`: The number of days for postnatal care (PNC) visit follow-up.

`FOLLOWUP_CHILD_VISIT_IN_DAYS`: The number of days for child visit follow-up.

`FOLLOWUP_CALL_ATTEMPTS`: The number of call attempts for follow-ups.

`SCREENING_FOLLOWUP_REMAINING_DAYS`: The number of days remaining for a screening follow-up.

`ASSESSMENT_FOLLOWUP_REMAINING_DAYS`: The number of days remaining for an assessment follow-up.

`MEDICAL_REVIEW_FOLLOWUP_REMAINING_DAYS`: The number of days remaining for a medical review follow-up.

`LOST_TO_FOLLOWUP_REMAINING_DAYS`: The number of days before considering a patient as lost to follow-up.

`RETRY_CALL_ATTEMPTS`: The number of retry attempts for follow-up calls.


## Alternative solution to AWS services using open-source and free service.

-   Storage Service - [MinIO](Open Source)
-   Mail Service - [SendGrid](Free service)
-   SMS Service - [Twilio](Free service)

> Note: When you choose to use the open-source MinIO service for storage.

### MinIO Storage service Setup

- To install [MinIO in Linux] run the following commands or click [Min.io site -Download].
```sh
  wget https://dl.min.io/server/minio/release/linux-amd64/archive/minio_20230916010147.0.0_amd64.deb -O minio.deb
  sudo dpkg -i minio.deb
  mkdir ~/minio
  minio server ~/minio --console-address :9090
```
- To install [MinIO in Windows]
- To install [MinIO in MacOS]

> Note: If you are using the min.io service, please enter the corresponding values for the fields `URL_MINIO`, `CONSOLE_URL_MINIO`, `ACCESS_NAME_MINIO`, `ACCESS_SECRET_MINIO` and `BUCKET_NAME_MINIO` in .env.

### Sendgrid Mail service Setup

> Note: When you choose to use the Sendgrid email free service, follow the below instructions.

- Add the sendgrid dependency in the file: spice-server/backend/notification-service/pom.xml
```sh
        <dependency>
            <groupId>com.sendgrid</groupId>
            <artifactId>sendgrid-java</artifactId>
            <version>4.0.1</version>
        </dependency>
```
- Replace the code in the file: spice-server/backend/notification-service/src/main/java/com/mdtlabs/coreplatform/notificationservice/service/impl/EmailServiceImpl.java from the line 105 to 137.
```sh
         Mail mail = new Mail();
         mail.setSubject(emailDto.getSubject());
         mail.addContent(new Content("text/HTML", emailDto.getBody()));
         mail.setFrom(new Email(mailFrom));
         Personalization personalization = new Personalization();
         personalization.addTo(new Email(StringUtils.isNotBlank(emailDto.getTo()) ? emailDto.getTo() : Constants.EMPTY));
         if (StringUtils.isNotBlank(emailDto.getCc())) {
             personalization.addCc(new Email(emailDto.getCc()));
         }
         if (StringUtils.isNotBlank(emailDto.getBcc())) {
             personalization.addBcc(new Email(emailDto.getBcc()));
         }
         mail.addPersonalization(personalization);
         SendGrid sendGrid = new SendGrid(sendgridApikey);
         Request request = new Request();
         request.setMethod(Method.POST);
         request.setEndpoint("mail/send");
         request.setBody(mail.build());
         sendGrid.api(request);
```
- Visit the [Sendgrid] official site and create a free account to get the sendgrid api key.

> Note: If you are using the sendgrid service, please enter the corresponding values for the fields `MAIL_FROM` and `SENDGRID_API_KEY` in .env.

### Twilio SMS service Setup

> Note: When you choose to use the Twilio SMS free service, follow the below instructions.

- Visit the [Twilio] official site and create a free account to get the account sid, auth token and twilio phone number.

> Note: If you are using the twilio service, please enter the corresponding values for the fields `TWILIO_ACCOUNT_SID`, `TWILIO_AUTH_TOKEN and `TWILIO_FROM_PHONENO` in .env.

## Deployment

Run the following commands in the below path

Build a clean-install using maven
`/spice-2.0-server`

```sh
mvn clean install
```

Build docker images by the following command

```sh
docker-compose build
```

Once the images are built, run the docker containers by the following docker command

```sh
docker-compose up
```

## Validation

Once the deployment of the application is successful, you can confirm the connectivity of the Back-end by logging into
the application. To accomplish this, you may choose any API Testing tool. In this particular case, the Postman API
Platform was utilized as an example.

### Endpoint

`POST {{ip}}/auth-service/session`

### Request Body

This endpoint allows user to sign-in into the application.
The request body should be in the **form-data** format and include the following fields:

- `username`: superuser@test.com
- `password`:
  3901e08e724bb73a72137e03e2f03d54d70eaeea39f8a7e0459f15d9e80585ffd2159283b8324d72f7ba8c2aa2f0e82e5c1b685d7ef569e2ebad02d71ba4076e

#### The response contains two values: Authorization and TenantId. You must use these values in the subsequent requests.


[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)

[Git Official site]:<https://git-scm.com/book/en/v2/Getting-Started-Installing-Git>

[Java Official site]:<https://www.oracle.com/in/java/technologies/downloads/#java17>

[Maven Official site - Download]: <https://maven.apache.org/download.cgi>

[Maven Official site - Install]:<https://maven.apache.org/install.html>

[Redis Official Docs]:<https://redis.io/docs/getting-started/installation/>

[Docker Official Docs]: <https://docs.docker.com/engine/install/>

[Docker Compose Official Docs]: <https://docs.docker.com/compose/install/linux/>

[Maven Official site -Download]: <https://maven.apache.org/download.cgi>

[MinIO in Linux]: <https://min.io/docs/minio/linux/index.html>

[MinIO in Windows]: <https://min.io/docs/minio/windows/index.html>

[MinIO in MacOS]: <https://min.io/docs/minio/macos/index.html>

[MinIO]: <https://min.io/>

[Sendgrid]: <https://sendgrid.com/free/>

[Twilio]: <https://www.twilio.com/try-twilio>

[HAPI FHIR JPA Server]: <https://github.com/Medtronic-LABS/hapi-fhir-jpaserver-starter>

[SPICE DOCUMENTATION](https://spice.docs.medtroniclabs.org/overview/what-is-spice)