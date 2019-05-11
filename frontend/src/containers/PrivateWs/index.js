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
    wsRef.current.sendMessage("929292"); // Send user id
  };

  return (
    <div>
      <button onClick={sendMessage}>Test private</button>
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
