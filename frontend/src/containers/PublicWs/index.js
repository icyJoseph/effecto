import React, { useRef } from "react";
import Websocket from "react-websocket";

export function PublicWs() {
  const wsRef = useRef();
  const handleConnection = () => {
    console.log("connected to server");
  };

  const handleMessage = data => {
    console.log("message", data);
  };

  const createMeeting = () => {
    wsRef.current.sendMessage(
      JSON.stringify({
        "command":"create_meeting", 
        "data":{
          "name":"0202020", 
          "agenda":[
            {"time": 1557560112000, "title":"What candy to buy"}
          ]
        }
      })
    );
  };

  const joinMeeting = () => {
    wsRef.current.sendMessage(
      JSON.stringify({
        "command":"join", 
        "data":{
          "id":"929292",
          "group":"0202020"
        }
      })
    );
  };

  const sendMessage = () => {
    wsRef.current.sendMessage(
      JSON.stringify({
        "command":"send", 
        "data":{
          "id":"929292",
          "group":"0202020",
          "message":"I suggest Riesen"
        }
      })
    );
  };

  return (
    <div>
      <button onClick={createMeeting}>createMeeting</button>
      <button onClick={joinMeeting}>joinMeeting</button>
      <button onClick={sendMessage}>sendMessage</button>
      <Websocket
        url="ws://localhost:8080/websocket"
        onOpen={handleConnection}
        onMessage={handleMessage}
        ref={wsRef}
      />
    </div>
  );
}

export default PublicWs;
