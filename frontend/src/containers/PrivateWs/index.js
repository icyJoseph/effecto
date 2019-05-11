import React, { useState, useEffect, useRef } from "react";
import Websocket from "react-websocket";
import { useSelector } from "react-redux";

export function PrivateWs() {
  const wsRef = useRef();
  const [valid, setValid] = useState(false);
  const { refresh } = useSelector(({ auth }) => auth);

  useEffect(() => {
    if (refresh) {
      const now = new Date();
      const refreshTime = new Date(refresh);
      if (now - refreshTime < 0) {
        setValid(true);
      }
    }
  }, [refresh]);

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
    valid && (
      <Websocket
        url="ws://localhost:8080/websocketserver"
        onOpen={handleConnection}
        onMessage={handleMessage}
        ref={wsRef}
      />
    )
  );
}

export default PrivateWs;
