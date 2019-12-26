import requests
import subprocess


def build_docs():
    with subprocess.Popen(
            ['node_modules/.bin/edp', '-n'],
            stdout=subprocess.PIPE
    ) as elmdocs:
        i = 0
        package = ''
        bind = ''
        version = ''
        while i < 2:
            line = elmdocs.stdout.readline().decode()
            print(line)
            if line.startswith('Previewing'):
                _, package, version, *_ = line.split(' ')
                i += 1

            if line.startswith('Browse'):
                _, address, *_ = line.split(' ')
                bind = address.strip('<>')
                i += 1

            if i > 2:
                break

        url = f'{bind}/packages/{package}/{version}/docs.json'

        print(f'navigating {url}')
        response = requests.get(url)
        response.raise_for_status()
        print(response.content)
        with open('data/docs.json', 'wb') as f:
            f.write(response.content)

        elmdocs.terminate()


if __name__ == '__main__':
    build_docs()
