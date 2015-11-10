alias reload!='. ~/.zshrc'
alias ll='ls -al --color=auto'
alias sagi='sudo apt-get install'
alias sasearch='sudo apt-cache search'
alias psgrep='ps -ef | grep -v grep | grep '
alias e='emacsclient -t'
alias ec='emacsclient -c'
alias vim='emacsclient -t'
alias vi='emacsclient -t'
alias k='gitk --all &'
alias vssh="vagrant ssh"

alias lmp-verify="mvn verify -P local-enterprise-test-db -am \
-DskipPreTestTeardown=false -DskipPostTestTeardown=true -DskipDesignTests=false \
-DskipTransactionTests=false -DskipMulticastTests=false -DskipTestCoverage=false \
-Dtoybox.limiting_service_host=http://127.0.0.1:9091/ \
-Dtoybox.code_manager_host=http://127.0.0.1:9091/ -pl ControlCenter"

alias lmp-package="mvn package -P source-mode -am \
-DskipPreTestTeardown=false -DskipPostTestTeardown=true -DskipDesignTests=false \
-DskipTransactionTests=false -DskipMulticastTests=false -DskipTestCoverage=false \
-Dtoybox.limiting_service_host=http://127.0.0.1:9091/ \
-Dtoybox.code_manager_host=http://127.0.0.1:9091/ -pl ControlCenter"

alias lmp-install="mvn install -P local-enterprise-test-db -am \
-DskipPreTestTeardown=false -DskipPostTestTeardown=true -DskipDesignTests=false \
-DskipTransactionTests=false -DskipMulticastTests=false -DskipTestCoverage=false \
-Dtoybox.limiting_service_host=http://127.0.0.1:9091/ \
-Dtoybox.code_manager_host=http://127.0.0.1:9091/"

alias lmp-deploy="mvn clean deploy -Dbuild.number=69 -P local-enterprise-db \
-DskipPreTestTeardown=true -DskipPostTestTeardown=true -DskipITs=true \
-DskipMulticastTests=true -DskipTestCoverage=true \
-Dport=2200"
# -DdeployUrl=scp://vagrant:vagrant@lmp-local:2200/opt/LMP/lib

# -pl RsDesigner \


alias lmp-designer="mvn exec:java \
--also-make \
-Dexec.mainClass=\"com.rewardstream.rsdesigner.RsDesigner\" \
-Dapi.port=8082 -Dtoybox.code_manager_host=http://127.0.0.1:9091/ \
-DLMP_USERNAME=admin -DLMP_PASSWORD=admin1234 \
 -Dmulticast.ip= -Dmulticast.port=60000"

alias lmp-designer-jar="java \
-Dapi.port=8082 \
-Dtoybox.code_manager_host=http://127.0.0.1:9091/ \
-DLMP_USERNAME=admin \
-DLMP_PASSWORD=admin1234 \
-Dmulticast.ip= \
-Dmulticast.port=60000 \
-jar RsDesigner/target/RsDesigner-6.6.1-b69-local-edb.jar"

alias lmp-jetty="java \
-Djetty.port=8080 \
-Dtoybox.limiting_service_host=http://127.0.0.1:9091/ \
-Dtoybox.code_manager_host=http://127.0.0.1:9091/ \
-Dclient.environment.code=dev \
-Dadmin.environment.code=dev \
-Dclient.identifier=demo \
-Djava.net.preferIPv4Stack=true \
-Dldap_username=daniel-renfer \
-jar LMPJetty/target/LMPJetty.jar"
