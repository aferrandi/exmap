const app = Elm.Main.init({
    node: document.getElementById('elm'),
    flags: {
        uname: window.location.hash.slice(1) || '',
    },
})

let socket = null

app.ports.messages.subscribe((message) => {
    if (socket) {
        socket.send(JSON.stringify(message))
    } else {
        console.warn('socket not set')
    }
})

app.ports.connection.subscribe((port) => {
    connect(port);
});

function connect(port, attempts = 0) {
    if (socket) {
        socket.close()
        return
    }
    endpoint = 'ws://'+window.location.hostname+':'+port
    console.log('connect to '+endpoint)
    socket = new WebSocket(endpoint)
    socket.onopen = () => {
        attempts = 0
        app.ports.onWebSocketChange.send(true)
    }

    socket.onerror = (event) => {}

    socket.onclose = (event) => {
        app.ports.onWebSocketChange.send(false)
        socket = null

        const delay = Math.min(1000 * attempts, 10000)
        console.log(attempts, delay + 'ms')

        setTimeout(() => {
            connect(attempts + 1)
        }, delay)
    }

    socket.onmessage = (envelope) => {
        console.log('message from server:', envelope.data)
        app.ports.onServerMessage.send(envelope.data)
    }
}