import React, { useRef } from "react";
import Websocket from "react-websocket";

export function PrivateWs() {
  const wsRef = useRef();
  const handleConnection = () => {
    console.log("connected to server");
  };

  const handleMessage = data => {
    console.log("message", data);
  };

  const sendMessage = () => {
    wsRef.current.sendMessage(
      JSON.stringify({ id: "929292", group: "0202020" })
    );
  };

  return (
    <div>
      <button onClick={sendMessage}>Test</button>
      <Websocket
        url="ws://localhost:8080/websocketserver"
        onOpen={handleConnection}
        onMessage={handleMessage}
        ref={wsRef}
      />
    </div>
  );
}

export default PrivateWs;
