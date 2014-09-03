 if [ -f /etc/debian_version ] ; then
     echo "Upgrade on Debian/Ubuntu"
     sudo apt-get -y --force-yes purge supervisor
     sudo apt-get -y --force-yes purge nginx-full
     sudo apt-get -y --force-yes purge mongodb mongodb-server mongodb-clients mongodb-dev
     sudo apt-get -y --force-yes purge tomcat7
     sudo apt-get -y --force-yes purge gunicorn

     sudo apt-get -y --force-yes autoremove
 elif [ -f /etc/redhat-release ] ; then
     echo "Upgrade on Redhat/CentOS"
     sudo yum -y erase mongodb mongodb-server
 fi
