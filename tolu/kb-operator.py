from datetime import timedelta,datetime
import airflow
from airflow.models import DAG
from airflow.providers.cncf.kubernetes.operators.kubernetes_pod import KubernetesPodOperator
from airflow.operators.python_operator import PythonOperator


YESTERDAY = datetime.now() - timedelta(days=1)

default_args = {
    'start_date': YESTERDAY
}

with DAG(
    'kubernetes-test',
    default_args=default_args,
    schedule_interval=timedelta(days=1)) as dag:

    

    #config for node 1
    t1 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task1',
        cmds=["python","./code.py","./docs/1.txt"]

    )
    #config for node 2
    t2 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task2',
        cmds=["python","./code.py","./docs/2.txt"]

    )
    #config for node 3
    t3 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/3.txt"]

    )
    #config for node 4
    t4 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/4.txt"]

    )
    #config for node 5
    t5 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/5.txt"]
    )
    #config for node 6
    t6 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/6.txt"]

    )
    #config for node 7
    t7 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/7.txt"]

    )
    #config for node 8
    t8 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/8.txt"]

    )
    #config for node 9
    t9 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/9.txt"]

    )
    #config for node 10
    t10 = KubernetesPodOperator(
        namespace='default',
        image='python3.7',
        name='composer-fluentd-daemon-25gzq',
        task='task3',
        cmds=["python","./code.py","./docs/10.txt"]

    )

    #this will be the daughter node responsible for computing
    #the results from the other 10 nodes
    t11 = {}

    [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10] >> t11        