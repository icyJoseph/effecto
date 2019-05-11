import React from "react";
import Websocket from "react-websocket";

export function PublicWs() {
  const handleConnection = () => {
    console.log("connected");
  };

  const handleMessage = data => {
    console.log("message", data);
  };

  return (
    <Websocket
      url="ws://localhost:8080/websocket"
      onOpen={handleConnection}
      onMessage={handleMessage}
    />
  );
}

export default PublicWs;
